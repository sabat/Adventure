C  Adventure - initialization
C
C 30-Aug-93	RMS	Revised for portability
C
	SUBROUTINE INIT
	IMPLICIT INTEGER (A-Z)
	INCLUDE 'aparam.for'
	LOGICAL BITSET
        INTEGER, DIMENSION(3) :: IJK
	BITSET(L,N) = IAND(COND(L),ISHFT(1,N)).NE.0

C Description of the database format.
C
C The data file contains several sections.  Each begins with a line containing
C a number identifying the section, and ends with a line containing "-1".
C
C Section 1: long form descriptions.  Each line contains a location number,
C	a comma, and a line of text.  The set of (necessarily adjacent) lines
C	whose numbers are X form the long description of location X.
C
C Section 2: short form descriptions.  Same format as long form.  Not all
C	places have short descriptions.
C
C Section 3: travel table.  Each line contains a location number (X), a
C	condition of motion (M), a location number (N), and a list of
C	motion numbers (see section 4).  Each motion represents a verb
C	which will go to N if currently at X with conditions M.
C	location number (Y), and a list of motion numbers (see section 4).
C	Each motion represents a verb which will go to Y if currently at X.
C	M specifies the conditions on the motion.
C		if M = 0	it's unconditional.
C		if 0<M<100	it is done with m% probability.
C		if M = 100	unconditional, but forbidden to dwarves.
C		if 100<M<=200	he must be carrying object m-100.
C		if 200<M<=300	must be carrying or in same room as m-200.
C		if 300<M<=400	prop(m mod 100) must *not* be 0.
C		if 400<M<=500	prop(m mod 100) must *not* be 1.
C		if 500<M<=600	prop(m mod 100) must *not* be 2, etc.
C	N, in turn, is interpreted as follows.  
C		if N<=300	it is the location to go to.
C		if 300<N<=500	N-300 is used in a computed goto to
C				a section of special code.
C		if N>500	message n-500 from section 6 is printed,
C				and he stays wherever he is.
C	If the condition (if any) is not met, then the next *different*
C	"destination" value is used (unless it fails to meet *its* conditions,
C	in which case the next is found, etc.).  Typically, the next dest will
C	be for one of the same verbs, so that its only use is as the alternate
C	destination for those verbs.  For instance:
C		15	110,022	29	31	34	35	23	43
C		15	000,014	29
C	This says that, from loc 15, any of the verbs 29, 31, etc., will take
C	him to 22 if he's carrying object 10, and otherwise will go to 14.
C		11	303,008	49
C		11	000,009	50
C	This says that, from 11, 49 takes him to 8 unless PROP(3)=0, in which
C	case he goes to 9.  Verb 50 takes him to 9 regardless of PROP(3).
C
C Section 4: vocabulary.  Each line contains a number (N), a tab, and an
C	eight-letter word.  Call M=N/1000.
C		if M = 0	the word is a motion verb for use in
C				travelling (see section 3). 
C		if M = 1	the word is an object.
C		if M = 2	the word is an action verb (such as "carry"
C				or "attack"). 
C		if M = 3	the word is a special case verb (such as
C				"dig") and n mod 1000 is an index into
C				section 6.
C	Objects from 50 to (currently, anyway) 79 are considered treasures
C	(for pirate, closeout).
C
C Section 5: object descriptions.  Each line contains a number (N), a tab,
C	and a message.  If N is from 1 to 100, the message is the "inventory"
C	message for object N.  Otherwise, N should be 000, 100, 200, etc., and
C	the message should be the description of the preceding object when its
C	PROP value is N/100.  The N/100 is used only to distinguish multiple
C	messages from multi-line messages; the PROP info actually requires all
C	messages for an object to be present and consecutive.  Properties which
C	produce no message should be given the message ">$<".
C
C Section 6: arbitrary messages.  Same format as sections 1, 2, and 5, except
C	the numbers bear no relation to anything (except for special verbs
C	in section 4).
C
C Section 7: object locations.  Each line contains an object number and its
C	initial location (zero (or omitted) if none).  If the object is
C	immovable, the location is followed by a "-1".  If it has two locations
C	(e.g. the grate) the first location is followed with the second, and
C	the object is assumed to be immovable.
C
C Section 8: action defaults.  Each line contains an "action-verb" number and
C	the index (in section 6) of the default message for the verb.
C
C Section 9: liquid assets, etc.  Each line contains a number (N) and up to 20
C	location numbers.  Bit N (where 0 is the units bit) is set in COND(LOC)
C	for each loc given.  The COND bits currently assigned are:
C		0	light
C		1	if bit 2 is on: on for oil, off for water
C		2	liquid asset, see bit 1
C		3	pirate doesn't go here unless following player
C	Other bits are used to indicate areas of interest to "HINT" routines:
C		4	trying to get into cave
C		5	trying to catch bird
C		6	trying to deal with snake
C		7	lost in maze
C		8	pondering dark room
C		9	at witt's end
C	COND(LOC) is set to 2, overriding all other bits, if LOC has forced
C	motion.
C
C Section 10: class messages.  Each line contains a number (N), a tab, and a
C	message describing a classification of player.  The scoring section
C	selects the appropriate message, where each message is considered to
C	apply to players whose scores are higher than the previous N but not
C	higher than this N.  Note that these scores probably change with every
C	modification (and particularly expansion) of the program.
C
C Section 11: hints.  Each line contains a hint number (corresponding to a
C	COND bit, see section 9), the number of turns he must be at the right
C	LOC(s) before triggering the hint, the points deducted for taking the
C	hint, the message number (section 6) of the question, and the message
C	number of the hint.  These values are stashed in the HINTS array.
C	HNTMAX is set to the max hint number (<= HNTSIZ).  Numbers 1-3 are
C	unusable since COND bits are otherwise assigned, so 2 is used to
C	remember if he's read the clue in the repository, and 3 is used to
C	remember whether he asked for instructions (gets more turns, but loses
C	points).
C
C Section 0: end of database.

C Open the data base file.
C Init the random number generator.
C
        OPEN (UNIT=1,FILE='atext.txt',ACCESS='SEQUENTIAL',
     1    STATUS='OLD',FORM='FORMATTED',ERR=2000) 
C    1    STATUS='OLD',READONLY,FORM='FORMATTED',ERR=2000)
	CALL IDATE(IJK)
	I = IOR((IJK(1)*16)+IJK(3),1)
	J = IOR((IJK(2)*16)+IJK(3),1)
	CALL INIRND(IJK(1),IJK(2))
	VCOUNT = 0
	PRINT 1000,VMAJ,VMIN,VEDIT
1000	FORMAT(' [Initializing Adventure V',I1,'.',I1,A,$)
C
C  Clear out the various text-pointer arrays.  All text is stored in arrays
C  TRECNO and TLINES.  The text-pointer arrays contain subscripts into these
C  arrays.
C	STEXT(N)	short description of location N.
C 	LTEXT(N)	long description of location N.
C	PTEXT(N)	points to message for PROP(N) = 0.
C			Successive messages are found by chasing pointers.
C	RTEXT(N)	section 6's stuff.
C	CTEXT(N)	points to a player-class message.
C  We also clear COND.  SEE description of section 9 for details.
C
10	DO 500 I = 1,TABSIZ
	KTAB(I) = 0
500	ATAB(I) = ' '
	DO 510 I = 1,OBJMAX
	PTEXT(I) = 0
	PROP(I) = 0
	PLAC(I) = 0
	PLACE(I) = 0
	FIXD(I) = 0
	FIXED(I) = 0
	LINK(I) = 0
510	LINK(I+OBJMAX) = 0
	DO 520 I = 1,RTXSIZ
520	RTEXT(I) = 0
	DO 530 I = 1,CLSSIZ
530	CTEXT(I) = 0
	DO 540 I = 1,VRBSIZ
540	ACTSPK(I) = 0
	DO 550 I = 1,LOCSIZ
	KEY(I) = 0
	ABB(I) = 0
	ATLOC(I) = 0
	STEXT(I) = 0
	LTEXT(I) = 0
550	COND(I) = 0

C
	LINUSE = 1
	TRVS = 1
	CLSSES = 1
C
C Start new data section.  SECT is the section number.
C
1002	READ (1,1003) SECT
1003	FORMAT(I5)
	IF (SECT.NE.0) PRINT 930,SECT
930	FORMAT('+',I3,$)
	OLDLOC = -1
	GOTO(1100,1004,1004,1030,1040,1004,1004,1050,1060,1070,1004,
     1   1080) (SECT+1)
C	      (0)  (1)  (2)  (3)  (4)  (5)  (6)  (7)  (8)  (9)  (10)
C	     (11)
	CALL BUG(9)
C
C Sections 1, 2, 5, 6, 10.  Read messages and set up pointers.
C
1004	READ (1,1005) TRECNO(LINUSE),TLINES(LINUSE)
	LOC = TRECNO(LINUSE)
1005	FORMAT(I5,A)
	IF(LOC .EQ. -1) GO TO 1002
	IF(LOC .EQ. OLDLOC) GO TO 1020
	IF(SECT.EQ.10) GOTO 1012
	IF(SECT.EQ.6) GOTO 1011
	IF(SECT.EQ.5) GOTO 1010
	IF(SECT.EQ.1) GOTO 1008
C
	IF(LOC.GT.LOCSIZ) CALL BUG(11)
	STEXT(LOC) = LINUSE
	GOTO 1020
C
1008	IF(LOC.GT.LOCSIZ) CALL BUG(11)
	LTEXT(LOC) = LINUSE
	GOTO 1020
C
1010	IF(LOC.GT.0.AND.LOC.LE.OBJMAX) PTEXT(LOC) = LINUSE
	GOTO 1020
C
1011	IF(LOC.GT.RTXSIZ) CALL BUG(6)
	RTEXT(LOC) = LINUSE
	GOTO 1020
C
1012	IF(CLSSES.GT.CLSSIZ) CALL BUG(12)
	CTEXT(CLSSES) = LINUSE
	CVAL(CLSSES) = LOC
	CLSSES = CLSSES+1
C
1020	OLDLOC = LOC
	IF(LINUSE.GE.FILSIZ) CALL BUG(2)
	LINUSE = LINUSE+1
	GOTO 1004
C
C The stuff for section 3 is encoded here.  Each "from-location" gets a
C contiguous section of the TRAVEL array.  Each entry in TRAVEL is
C keyword (from section 4, motion verbs), and is negated if
C this is the last entry for this location.  KEY(N) is the index in travel
C of the first option at location N.
C
C Special conditions on travel are encoded in the corresponding
C entries of TRVCON.  The new location is in TRVLOC.
C
1030	READ (1,1031) LOC,J,NEWLOC,TK
1031	FORMAT(99I6)
	IF(LOC.EQ.-1) GOTO 1002
	IF(KEY(LOC).NE.0) GOTO 1033
	KEY(LOC) = TRVS
	GOTO 1035
1033	TRAVEL(TRVS-1) = -TRAVEL(TRVS-1)
1035	DO 1037 L = 1,HNTSIZ
	IF(TK(L).EQ.0) GOTO 1039
	TRAVEL(TRVS) = TK(L)
	TRVLOC(TRVS) = NEWLOC
	TRVCON(TRVS) = J
	TRVS = TRVS+1
	IF(TRVS.EQ.TRVSIZ) CALL BUG(3)
1037	CONTINUE
1039	TRAVEL(TRVS-1) = -TRAVEL(TRVS-1)
	GOTO 1030
C
C Here we read in the vocabulary.  KTAB(N) is the word number, ATAB(N) is
C the corresponding word.  The -1 at the end of section 4 is left in KTAB
C as an end-marker.
C
1040	DO 1042 TABNDX = 1,TABSIZ
1043	READ (1,1041) KTAB(TABNDX),ATAB(TABNDX)
1041	FORMAT(I6,A)
	IF(KTAB(TABNDX).EQ.-1) GOTO 1002
1042	CONTINUE
	CALL BUG(4)
C
C Read in the initial locations for each object.  Also the immovability info.
C PLAC contains initial locations of objects.  FIXD is -1 for immovable
C objects (including the snake), or = second loc for two-placed objects.
C
1050	READ (1,1031) OBJ,J,K
	IF(OBJ.EQ.-1) GOTO 1002
	IF(OBJ.GT.OBJMAX) CALL BUG(13)
	PLAC(OBJ) = J
	FIXD(OBJ) = K
	GOTO 1050
C
C Read default message numbers for action verbs, store in ACTSPK.
C
1060	READ (1,1031) VERB,J
	IF(VERB.EQ.-1) GOTO 1002
	IF(VERB.GT.VRBSIZ) CALL BUG(10)
	ACTSPK(VERB) = J
	VCOUNT = MAX0(VERB,VCOUNT)
	GOTO 1060
C
C Read info about available liquids and other conditions, store in COND.
C
1070	READ (1,1031) K,TK
	IF(K.EQ.-1) GOTO 1002
	DO 1071 I = 1,HNTSIZ
	LOC = TK(I)
	IF(LOC.EQ.0) GOTO 1070
	IF(BITSET(LOC,K)) CALL BUG(8)
1071	COND(LOC) = COND(LOC)+ISHFT(1,K)
	GOTO 1070
C
C Read data for hints.
C
1080	HNTMAX = 0
1081	READ (1,1031) K,TK
	IF(K.EQ.-1) GOTO 1002
	IF(K.LT.0 .OR. K.GT.HNTSIZ) CALL BUG(7)
	DO 1083 I = 1,4
1083	HINTS(K,I) = TK(I)
	HNTMAX = MAX0(HNTMAX,K)
	GOTO 1081

C Done.
C
1100	CLOSE (UNIT=1)
	PRINT 1101
1101	FORMAT('+]')
C
C Having read in the database, certain things are now constructed.  PROPS are
C set to zero.  We finish setting up COND by checking for forced-motion travel
C entries.  The PLAC and FIXD arrays are used to set up ATLOC(N) as the first
C object at location N, and LINK(OBJ) as the next object at the same location
C as OBJ.  (OBJ>100 indicates that FIXED(OBJ-100)=LOC; LINK(OBJ) is still the
C correct link to use.)  ABB is zeroed; it controls whether the abbreviated
C description is printed.  Counts mod 5 unless LOOK is used.
C
C If the first motion verb is 1 (illegal), then this is a forced
C motion entry.
C
5000	DO 1102 I = 1,LOCSIZ
	IF(LTEXT(I).EQ.0 .OR. KEY(I).EQ.0) GOTO 1102
	K = KEY(I)
	IF(IABS(TRAVEL(K)).EQ.1) COND(I) = 2
1102	CONTINUE
C
C Set up the ATLOC and LINK arrays as described above.  We'll use the DROP
C subroutine, which prefaces new objects on the lists.  Since we want things
C in the other order, we'll run the loop backwards.  If the object is in two
C locs, we drop it twice.  This also sets up PLACE and FIXED as copies of
C PLAC and FIXD.  Also, since two-placed objects are typically best
C described last, we'll drop them first.
C
	DO 1106 I = 1,OBJMAX
	K = OBJMAX+1-I
	IF(FIXD(K).LE.0) GOTO 1106
	CALL DROP(K+OBJMAX,FIXD(K))
	CALL DROP(K,PLAC(K))
1106	CONTINUE
C
	DO 1107 I = 1,OBJMAX
	K = OBJMAX+1-I
	FIXED(K) = FIXD(K)
1107	IF(PLAC(K).NE.0 .AND. FIXD(K).LE.0) CALL DROP(K,PLAC(K))
C
C Treasures, as noted earlier, are objects 50 through MAXTRS (currently 79).
C Their PROPS are initially -1, and are set to 0 the first time they are
C described.  TALLY keeps track of how many are not yet found, so we know
C when to close the cave.  TALLY2 counts how many can never be found (e.g. if
C lost bird or bridge).
C
	MAXTRS = 79
	TALLY = 0
	TALLY2 = 0
	DO 1200 I = 50,MAXTRS
	IF(PTEXT(I).NE.0) PROP(I) = -1
1200	TALLY = TALLY-PROP(I)
C
C Clear the hint stuff.  HINTLC(I) is how long he's been at LOC with COND bit
C I.  HINTED(I) is true iff hint I has been used.
C
	DO 1300 I = 1,HNTMAX
	HINTED(I) = .FALSE.
1300	HINTLC(I) = 0
C
C D       PRINT 931,TABNDX,TABSIZ,VCOUNT,VRBSIZ,CLSSES,CLSSIZ,
C D     1  HNTMAX,HNTSIZ,TRVS,TRVSIZ,LINUSE,FILSIZ
C D931	FORMAT(' Used vs Max table values:'/
C D     1  1X,I5,' OF ',I5,' VOCAB entries'/
C D     2  1X,I5,' OF ',I5,' VERB entries'/
C D     3  1X,I5,' OF ',I5,' CLASS entries'/
C D     4  1X,I5,' OF ',I5,' HINT entries'/
C D     5  1X,I5,' OF ',I5,' TRAVEL entries'/
C D     6  1X,I5,' OF ',I5,' FILE records'/
C D     9  )
C
C Define some handy mnemonics.  These correspond to object numbers.
C
	CALL VOCAB('KEYS',1,KEYS)
	CALL VOCAB('LAMP',1,LAMP)
	CALL VOCAB('GRATE',1,GRATE)
	CALL VOCAB('CAGE',1,CAGE)
	CALL VOCAB('ROD ',1,ROD)
	ROD2 = ROD+1
	CALL VOCAB('STEPS',1,STEPS)
	CALL VOCAB('BIRD',1,BIRD)
	CALL VOCAB('DOOR',1,DOOR)
	CALL VOCAB('PILLOW',1,PILLOW)
	CALL VOCAB('SNAKE',1,SNAKE)
	CALL VOCAB('FISSURE',1,FISSUR)
	CALL VOCAB('TABLET',1,TABLET)
	CALL VOCAB('CLAM',1,CLAM)
	CALL VOCAB('OYSTER',1,OYSTER)
	CALL VOCAB('MAGAZINE',1,MAGZIN)
	CALL VOCAB('DWARF',1,DWARF)
	CALL VOCAB('KNIFE',1,KNIFE)
	CALL VOCAB('FOOD',1,FOOD)
	CALL VOCAB('BOTTLE',1,BOTTLE)
	CALL VOCAB('WATER',1,WATER)
	CALL VOCAB('OIL ',1,OIL)
	CALL VOCAB('PLANT',1,PLANT)
	PLANT2 = PLANT+1
	CALL VOCAB('AXE ',1,AXE)
	CALL VOCAB('MIRROR',1,MIRROR)
	CALL VOCAB('DRAGON',1,DRAGON)
	CALL VOCAB('CHASM',1,CHASM)
	CALL VOCAB('TROLL',1,TROLL)
	TROLL2 = TROLL+1
	CALL VOCAB('BEAR',1,BEAR)
	CALL VOCAB('MESSAGE',1,MESSAG)
	CALL VOCAB('VENDING',1,VEND)
	CALL VOCAB('BATTERY',1,BATTER)
C
C Objects from 50 through whatever are treasures.  Here are a few.
C
	CALL VOCAB('GOLD',1,NUGGET)
	CALL VOCAB('COINS',1,COINS)
	CALL VOCAB('CHEST',1,CHEST)
	CALL VOCAB('EGGS',1,EGGS)
	CALL VOCAB('TRIDENT',1,TRIDNT)
	CALL VOCAB('VASE',1,VASE)
	CALL VOCAB('EMERALD',1,EMRALD)
	CALL VOCAB('PYRAMID',1,PYRAM)
	CALL VOCAB('PEARL',1,PEARL)
	CALL VOCAB('RUG ',1,RUG)
	CALL VOCAB('CHAIN',1,CHAIN)
	CALL VOCAB('SPICES',1,SPICES)
C
C These are motion-verb numbers.
C
	CALL VOCAB('BACK',0,BACK)
	CALL VOCAB('LOOK',0,LOOK)
	CALL VOCAB('CAVE',0,CAVE)
	CALL VOCAB('NULL',0,NULL)
	CALL VOCAB('ENTRANCE',0,ENTRNC)
	CALL VOCAB('DEPRESSI',0,DPRSSN)
	CALL VOCAB('STREAM',0,STREAM)
C
C And some action verbs.
C
	CALL VOCAB('SAY ',2,SAY)
	CALL VOCAB('LOCK',2,LOCK)
	CALL VOCAB('THROW',2,THROW)
	CALL VOCAB('FIND',2,FIND)
	CALL VOCAB('INVENTOR',2,INVENT)
C
C Initialise the dwarves.  DLOC is loc of dwarves, hard-wired in.  ODLOC is
C prior loc of each dwarf, initially garbage.  DALTLC is alternate initial loc
C for dwarf, in case one of them starts out on top of the adventurer.  (No 2
C of the 5 initial locs are adjacent.)  DSEEN is true if dwarf has seen him.
C DFLAG controls the level of activation of all this:
C     0  no dwarf stuff yet (wait until reaches Hall of Mists)
C     1  reached Hall of Mists, but hasn't met first dwarf
C     2  met first dwarf, others start moving, no knives thrown yet
C     3  a knife has been thrown (first set always misses)
C     3+  dwarves are mad (increases their accuracy)
C Sixth dwarf is special (the pirate).  He always starts at his chest's
C Eventual location inside the maze.  This loc is saved in CHLOC for ref.
C The dead end in the other maze has its loc stored in CHLOC2.
C
	CHLOC = 114
	CHLOC2 = 140
	DO 1700 I = 1,DWFMAX
1700	DSEEN(I) = .FALSE.
	DFLAG = 0
	DLOC(1) = 19
	DLOC(2) = 27
	DLOC(3) = 33
	DLOC(4) = 44
	DLOC(5) = 64
	DLOC(6) = CHLOC
	DALTLC = 18
C
C other random flags and counters, as follows:
C	TURNS	tallies how many commands he's given (ignores yes/no)
C	LIMIT	lifetime of lamp (not set here)
C	KNFLOC	0 if no knife here, loc if knife here, -1 after caveat
C	DETAIL	how often we've said "Not allowed to give more detail"
C	ABBNUM	how often we should print non-abbreviated descriptions
C	MAXDIE	number of reincarnation messages available (up to 5)
C	NUMDIE	number of times killed so far
C	HOLDNG	number of objects being carried
C	DKILL	number of dwarves killed (unused in scoring, needed for msg)
C	FOOBAR	current progress in saying "FEE FIE FOE FOO".
C	BONUS	used to determine amount of bonus if he reaches closing
C	CLOCK1	number of turns from finding last treasure till closing
C	CLOCK2	number of turns from first warning till blinding flash
C	Logicals were explained earlier
C
	TURNS = 0
	LMWARN = .FALSE.
	KNFLOC = 0
	DETAIL = 0
	ABBNUM = 5
	DO 1800 I = 0,4
1800	IF(RTEXT(2*I+81).NE.0) MAXDIE = I+1
	NUMDIE = 0
	HOLDNG = 0
	DKILL = 0
	FOOBAR = 0
	BONUS = 0
	CLOCK1 = 30
	CLOCK2 = 50
	CLOSNG = .FALSE.
	PANIC = .FALSE.
	CLOSED = .FALSE.
	GAVEUP = .FALSE.
	SCORNG = .FALSE.
	RETURN
C
C Error opening data base file.
C
2000	PRINT 2010
2010	FORMAT(' I can''t open the data base.')
	CALL EXIT
	END
