C  Adventure - main program
C
	SUBROUTINE MAIN
	IMPLICIT INTEGER (A-Z)
	INCLUDE 'aparam.for'
	LOGICAL YES,TOTING,HERE,AT,BITSET,DARK,WZDARK,YEA,VERBOSITY
        LOGICAL FORCED
        LOGICAL, DIMENSION(100) :: PCT
	CHARACTER*8 WORD1,WORD2,OLDWORD1,OLDWORD2
C
C  Statement functions
C
C  TOTING(OBJ)	= true if the OBJ is being carried
C  HERE(OBJ)	= true if the OBJ is at "LOC" (or is being carried)
C  AT(OBJ)	= true if on either side of two-placed OBJ
C  LIQ(DUMMY)	= object number of liquid in bottle
C  LIQLOC(LOC)	= object number of liquid (if any) at LOC
C  BITSET(L,N)	= true if COND(L) has bit N set (bit 0 is units bit)
C  FORCED(LOC)	= true if LOC moves without asking for input (COND=2)
C  DARK(DUMMY)	= true if location "LOC" is dark
C  PCT(N)       = true N% of the time (N integer from 0 to 100)
C
C  WZDARK says whether the LOC he's leaving was dark
C  LMWARN says whether he's been warned about lamp going dim
C  CLOSNG says whether its closing time yet
C  PANIC says whether he's found out he's trapped in the cave
C  CLOSED says whether we're all the way closed
C  GAVEUP says whether he exited via "QUIT"
C  SCORNG indicates to the SCORE routine whether we're doing a "SCORE" command
C  DEMO is true if this is a prime-time demonstration game
C  YEA is random YES/NO reply
C
	TOTING(OBJ)=PLACE(OBJ).EQ.-1
	HERE(OBJ)=PLACE(OBJ).EQ.LOC.OR.TOTING(OBJ)
	AT(OBJ)=PLACE(OBJ).EQ.LOC.OR.FIXED(OBJ).EQ.LOC
	LIQ2(PBOTL)=(1-PBOTL)*WATER+(PBOTL/2)*(WATER+OIL)
	LIQ(DUMMY)=LIQ2(MAX0(PROP(BOTTLE),-1-PROP(BOTTLE)))
	LIQLOC(LOC)=LIQ2((MOD(COND(LOC)/2*2,8)-5)*MOD(COND(LOC)/4,2)+1)
        BITSET(L,N)=IAND(COND(L),ISHFT(1,N)).NE.0
	FORCED(LOC)=COND(LOC).EQ.2
	DARK(DUMMY)=MOD(COND(LOC),2).EQ.0.AND.(PROP(LAMP).EQ.0.OR.
     1  .NOT.HERE(LAMP))
	PCT(N)=RND(100).LT.N
	VERBOSITY=.FALSE.
	OLDWORD1=''

C  Start-up, dwarf stuff
C
1	I=RND(1)
	HINTED(3)=YES(65,1,0)
	NEWLOC=1
	LOC = NEWLOC
	LIMIT=330
	IF(HINTED(3))LIMIT=1000
C
C  Can't leave cave once it's closing (except by main office).
C
2	IF(NEWLOC.GE.9.OR.NEWLOC.EQ.0.OR..NOT.CLOSNG)GOTO 71
	CALL RSPEAK(130)
	NEWLOC=LOC
	IF(.NOT.PANIC)CLOCK2=15
	PANIC=.TRUE.
C
C  See if a dwarf has seen him and has come from where he wants to go.  If so,
C  the dwarf's blocking his way.  If coming from place forbidden to pirate
C  (dwarves rooted in place) let him get out (and attacked).
C
71	IF(NEWLOC .EQ. LOC .OR. FORCED(LOC) .OR. BITSET(LOC,3)) GOTO 74
C 71      IF(NEWLOC.EQ.IOR(LOC,FORCED(LOC)).OR.BITSET(LOC,3))GOTO 74
	DO 73 I=1,5
	IF(ODLOC(I).NE.NEWLOC.OR..NOT.DSEEN(I))GOTO 73
	NEWLOC=LOC
	CALL RSPEAK(2)
	GOTO 74
73	CONTINUE
74	LOC=NEWLOC
C
C  Dwarf stuff.  See earlier comments for description of variables.  Remember
C  sixth dwarf is pirate and is thus very different except for motion rules.
C
C  First off, don't let the dwarves follow him into a pit or a wall.  Activate
C  the whole mess the first time he gets as far as the Hall of Mists (LOC 15).
C  If NEWLOC is forbidden to pirate (in particular, if it's beyond the Troll
C  Bridge), bypass dwarf stuff.  That way pirate can't steal return toll, and 
C  dwarves can't meet the bear.  Also means dwarves won't follow him into Dead
C  End in Maze, but c'est la vie.  They'll wait for him outside the Dead End.
C
	IF(LOC.EQ.0.OR.FORCED(LOC).OR.BITSET(NEWLOC,3))GOTO 2000
	IF(DFLAG.NE.0)GOTO 6000
	IF(LOC.GE.15)DFLAG=1
	GOTO 2000
C
C  When we encounter the first dwarf, we kill 0, 1, or 2 of the 5 dwarves.  If
C  any of the survivors is at LOC, replace him with the alternate.
C
6000	IF(DFLAG.NE.1)GOTO 6010
	IF(LOC.LT.15.OR.PCT(95))GOTO 2000
	DFLAG=2
	DO 6001 I=1,2
	J=1+RND(DWFMAX-1)
6001	IF(PCT(50))DLOC(J)=0
	DO 6002 I=1,5
	IF(DLOC(I).EQ.LOC)DLOC(I)=DALTLC
6002	ODLOC(I)=DLOC(I)
	CALL RSPEAK(3)
	CALL DROP(AXE,LOC)
	GOTO 2000
C
C  Things are in full swing.  Move each dwarf at random, except if he's seen us
C  he sticks with us.  Dwarves never go to LOCS <15.  If wandering at random,
C  they don't back up unless there's no alternative.  If they don't have to
C  move, they attack.  And, of course, dead dwarves don't do much of anything.
C
6010	DTOTAL=0
	ATTACK=0
	STICK=0
	DO 6030 I=1,DWFMAX
	IF(DLOC(I).EQ.0)GOTO 6030
	J=1
	KK=DLOC(I)
	KK=KEY(KK)
	IF(KK.EQ.0)GOTO 6016
6012	NEWLOC=TRVLOC(KK)
	IF(NEWLOC.GT.300.OR.NEWLOC.LT.15.OR.NEWLOC.EQ.ODLOC(I)
     1  .OR.(J.GT.1.AND.NEWLOC.EQ.TK(J-1)).OR.J.GE.HNTSIZ
     2  .OR.NEWLOC.EQ.DLOC(I).OR.FORCED(NEWLOC)
     3  .OR.(I.EQ.DWFMAX.AND.BITSET(NEWLOC,3))
     4  .OR.TRVCON(KK).EQ.100)GOTO 6014
	TK(J)=NEWLOC
	J=J+1
6014	KK=KK+1
	IF(TRAVEL(KK-1).GE.0)GOTO 6012
6016	TK(J)=ODLOC(I)
	IF(J.GE.2)J=J-1
	J=1+RND(J)
	ODLOC(I)=DLOC(I)
	DLOC(I)=TK(J)
	DSEEN(I)=(DSEEN(I).AND.LOC.GE.15)
     1  .OR.(DLOC(I).EQ.LOC.OR.ODLOC(I).EQ.LOC)
	IF(.NOT.DSEEN(I))GOTO 6030
	DLOC(I)=LOC
	IF(I.NE.DWFMAX)GOTO 6027
C
C  The pirate's spotted him.  He leaves him alone once we've found chest.
C  K counts if a treasure is here.  If not, and TALLY=TALLY2 plus one for
C  an unseen chest, let the pirate be spotted.
C
	IF(LOC.EQ.CHLOC.OR.PROP(CHEST).GE.0)GOTO 6030
	K=0
	DO 6020 J=50,MAXTRS
C
C  Pirate won't take pyramid from Plover Room or Dark Room (too easy!).
C
	IF(J.EQ.PYRAM.AND.(LOC.EQ.PLAC(PYRAM)
     1  .OR.LOC.EQ.PLAC(EMRALD)))GOTO 6020
	IF(TOTING(J))GOTO 6022
6020	IF(HERE(J))K=1
	IF(TALLY.EQ.TALLY2+1.AND.K.EQ.0.AND.PLACE(CHEST).EQ.0
     1  .AND.HERE(LAMP).AND.PROP(LAMP).EQ.1)GOTO 6025
	IF(ODLOC(DWFMAX).NE.DLOC(DWFMAX).AND.PCT(20))CALL RSPEAK(127)
	GOTO 6030
C
6022	CALL RSPEAK(128)
C
C  Don't steal chest back from troll!
C
	IF(PLACE(MESSAG).EQ.0)CALL MOVE(CHEST,CHLOC)
	CALL MOVE(MESSAG,CHLOC2)
	DO 6023 J=50,MAXTRS
	IF(J.EQ.PYRAM.AND.(LOC.EQ.PLAC(PYRAM)
     1  .OR.LOC.EQ.PLAC(EMRALD)))GOTO 6023
	IF(AT(J).AND.FIXED(J).EQ.0)CALL CARRY(J,LOC)
	IF(TOTING(J))CALL DROP(J,CHLOC)
6023	CONTINUE
6024	DLOC(DWFMAX)=CHLOC
	ODLOC(DWFMAX)=CHLOC
	DSEEN(DWFMAX)=.FALSE.
	GOTO 6030
C
6025	CALL RSPEAK(186)
	CALL MOVE(CHEST,CHLOC)
	CALL MOVE(MESSAG,CHLOC2)
	GOTO 6024
C
C  This threatening little dwarf is in the room with him!
C
6027	DTOTAL=DTOTAL+1
	IF(ODLOC(I).NE.DLOC(I))GOTO 6030
	ATTACK=ATTACK+1
	IF(KNFLOC.GE.0)KNFLOC=LOC
	IF(RND(1000).LT.95*(DFLAG-2))STICK=STICK+1
6030	CONTINUE
C
C  Now we know what's happening.  Let's tell the poor sucker about it.
C
	IF(DTOTAL.EQ.0)GOTO 2000
	IF(DTOTAL.EQ.1)GOTO 75
67      FORMAT(' There are ',I1,' threatening little dwarves in the'
     1  ,' room with you.')
        PRINT 67, DTOTAL
        GOTO 77
75      CALL RSPEAK(4)
77      IF(ATTACK.EQ.0)GOTO 2000
        IF(DFLAG.EQ.2)DFLAG=3
        IF(ATTACK.EQ.1)GOTO 79
        PRINT 78,ATTACK
78      FORMAT(' ',I1,' of them throw knives at you!')
        K=6
82      IF(STICK.GT.1)GOTO 83
        CALL RSPEAK(K+STICK)
        IF(STICK.EQ.0)GOTO 2000
        GOTO 84
68      FORMAT(' ',I1,' of them get you!')
83      PRINT 68,STICK
84      OLDLC2=LOC
        GOTO 99
C
79      CALL RSPEAK(5)
	K=52
	GOTO 82

C  Describe the current location and (maybe) get next command.
C
C  Print text for current LOC.
C
2000	IF(LOC.EQ.0)GOTO 99
	KK=STEXT(LOC)
	KENT=0
	IF (ABBNUM.NE.0) KENT=MOD(ABB(LOC),ABBNUM)
	IF (KENT.EQ.0.OR.KK.EQ.0.OR.VERBOSITY.EQV..TRUE.) KK=LTEXT(LOC)
	IF(FORCED(LOC).OR..NOT.DARK(0))GOTO 2001
	IF(WZDARK.AND.PCT(35))GOTO 90
	KK=RTEXT(16)
2001	IF(TOTING(BEAR))CALL RSPEAK(141)
	CALL SPEAK(KK)
	K=1
	IF(FORCED(LOC))GOTO 8
	IF(LOC.EQ.33.AND.PCT(25).AND..NOT.CLOSNG)CALL RSPEAK(8)
C
C  Print out descriptions of objects at this location.  If not closing and
C  property value is negative, tally off another treasure.  Rug is special
C  case; once seen, its PROP is 1 (dragon on it) till dragon is killed.
C  Similarly for chain; PROP is initially 1 (locked to bear).  These hacks
C  are because PROP=0 is needed to get full score.
C
	IF(DARK(0))GOTO 2012
	ABB(LOC)=ABB(LOC)+1
C  ATLOC is the subroutine that shows the objs at loc
	I=ATLOC(LOC)
2004	IF(I.EQ.0)GOTO 2012
	OBJ=I
	IF(OBJ.GT.OBJMAX)OBJ=OBJ-OBJMAX
	IF(OBJ.EQ.STEPS.AND.TOTING(NUGGET))GOTO 2008
	IF(PROP(OBJ).GE.0)GOTO 2006
	IF(CLOSED)GOTO 2008
	PROP(OBJ)=0
	IF(OBJ.EQ.RUG.OR.OBJ.EQ.CHAIN)PROP(OBJ)=1
	TALLY=TALLY-1
C  IF REMAINING TREASURES TOO ELUSIVE, ZAP HIS LAMP.
	IF(TALLY.EQ.TALLY2.AND.TALLY.NE.0)LIMIT=MIN0(35,LIMIT)
2006	KK=PROP(OBJ)
	IF(OBJ.EQ.STEPS.AND.LOC.EQ.FIXED(STEPS))KK=1
	CALL PSPEAK(OBJ,KK)
2008	I=LINK(I)
	GOTO 2004
C
2009	K=54
2010	SPK=K
2011	CALL RSPEAK(SPK)
C
2012	VERB=0
C Clearing OBJ keeps the hints from working.
C - Eric Dittman
C	OBJ=0
C
C  Check if this LOC is eligible for any hints.  If been here long enough,
C  branch to help section (on later page).  Hints all come back here eventually
C  to finish the loop.  Ignore "HINTS" < 4 (special stuff, see database notes).
C
2600	DO 2602 HINT=4,HNTMAX
	IF(HINTED(HINT))GOTO 2602
	IF(.NOT.BITSET(LOC,HINT))HINTLC(HINT)=-1
	HINTLC(HINT)=HINTLC(HINT)+1
	IF(HINTLC(HINT).LT.HINTS(HINT,1))GOTO 2602
C
C The section below was moved from outside the loop to avoid warnings.
C - Eric Dittman
C
C  HINTS
C
C  Come here if he's been long enough at required LOC(S) for some unused hint.
C  Hint number is in variable "HINT".  Branch to quick test for additional
C  conditions, then come back to do neat stuff.  GOTO 40010 if conditions are
C  met and we want to offer the hint.  GOTO 40020 to clear HINTLC back to zero,
C  40030 to take no action yet.
C
40000	GOTO (40400,40500,40600,40700,40800,40900)(HINT-3)
C	      CAVE  BIRD  SNAKE MAZE  DARK  WITT
	CALL BUG(27)
C
40010	HINTLC(HINT)=0
	IF(.NOT.YES(HINTS(HINT,3),0,54))GOTO 2602
	PRINT 40012,HINTS(HINT,2)
40012	FORMAT(' I am prepared to give you a hint, but it will cost you',
     1  I2,' points.')
	HINTED(HINT)=YES(175,HINTS(HINT,4),54)
	IF(HINTED(HINT).AND.LIMIT.GT.30)LIMIT=LIMIT+30*HINTS(HINT,2)
40020	HINTLC(HINT)=0
40030	GOTO 2602
C
C  Now for the quick tests.  See database description for one-line notes.
C
40400	IF(PROP(GRATE).EQ.0.AND..NOT.HERE(KEYS))GOTO 40010
	GOTO 40020
C
40500	IF(HERE(BIRD).AND.TOTING(ROD).AND.OBJ.EQ.BIRD)GOTO 40010
	GOTO 40030
C
40600	IF(HERE(SNAKE).AND..NOT.HERE(BIRD))GOTO 40010
	GOTO 40020
C
40700	IF(ATLOC(LOC).EQ.0.AND.ATLOC(OLDLOC).EQ.0
     1  .AND.ATLOC(OLDLC2).EQ.0.AND.HOLDNG.GT.1)GOTO 40010
	GOTO 40020
C
40800	IF(PROP(EMRALD).NE.-1.AND.PROP(PYRAM).EQ.-1)GOTO 40010
	GOTO 40020
C
40900	GOTO 40010
C
2602	CONTINUE
C
C  Kick the random number generator just to add variety to the chase.  Also,
C  if closing time, check for any objects being toted with PROP < 0 and set
C  the PROP to -1-PROP.  This way objects won't be described until they've
C  been picked up and put down separate from their respective piles.  Don't
C  tick CLOCK1 unless well into cave (and not at Y2).
C
	IF(.NOT.CLOSED)GOTO 2605
	IF(PROP(OYSTER).LT.0.AND.TOTING(OYSTER))
     1  CALL PSPEAK(OYSTER,1)
	DO 2604 I=1,OBJMAX
2604	IF(TOTING(I).AND.PROP(I).LT.0)PROP(I)=-1-PROP(I)
2605	WZDARK=DARK(0)
	IF(KNFLOC.GT.0.AND.KNFLOC.NE.LOC)KNFLOC=0
	I=RND(1)
	CALL GETIN(WORD1,WORD2)
	IF(WORD1.EQ.'AGAIN')GOTO 2645
	OLDWORD1=WORD1
	OLDWORD2=WORD2
C
C  Every input, check "FOOBAR" flag.  If zero, nothing's going on.  If pos,
C  make neg.  If neg, he skipped a word, so make it zero.
C
2608	FOOBAR=MIN0(0,-FOOBAR)
	TURNS=TURNS+1
C	IF(VERB.EQ.SAY .AND. WORD2.NE.' ')VERB=0
C	IF(VERB.EQ.SAY)GOTO 4090
	IF(TALLY.EQ.0.AND.LOC.GE.15.AND.LOC.NE.33)CLOCK1=CLOCK1-1
	IF(CLOCK1.EQ.0)GOTO 10000
	IF(CLOCK1.LT.0)CLOCK2=CLOCK2-1
	IF(CLOCK2.EQ.0)GOTO 11000
	IF(PROP(LAMP).EQ.1)LIMIT=LIMIT-1
	IF(LIMIT.LE.30.AND.HERE(BATTER).AND.PROP(BATTER).EQ.0
     1  .AND.HERE(LAMP))GOTO 12000
	IF(LIMIT.EQ.0)GOTO 12400
	IF(LIMIT.LT.0.AND.LOC.LE.8)GOTO 12600
	IF(LIMIT.LE.30)GOTO 12200
19999	K=43
	IF(LIQLOC(LOC).EQ.WATER)K=70
C
C  Do preliminary analysis of sentence to find certain special
C  cases, viz,
C
C  ENTER <WATER,STREAM>
C  ENTER <LOCATION>
C  <WATER,OIL> <PLANT,DOOR>
C
	CALL VOCAB(WORD1,-1,I)
	CALL VOCAB(WORD2,-1,J)
	IF(WORD1.NE.'ENTER') GO TO 2609
	IF(J .EQ. (WATER+1000)
     1  .OR. J .EQ. STREAM) GO TO 2010
	IF(WORD2 .NE. ' ') GO TO 2800
2609	IF((I .NE. (WATER+1000) .AND. I .NE. (OIL+1000))
     1  .OR. (J .NE. (PLANT+1000) .AND. J .NE. (DOOR+1000)))
     2  GO TO 2610
	WORD2='POUR'
2610	IF(WORD1 .EQ. 'WEST' .AND. PCT(10)) CALL RSPEAK(17)
2630	CALL VOCAB(WORD1,-1,I)
	IF(I.EQ.-1) GOTO 3000
	K=MOD(I,1000)
	KQ=I/1000+1
	IF(WORD1.EQ.'VERBOSE'.OR.WORD1.EQ.'VERBOSIT')GOTO 3100
2640	GOTO (8,5000,4000,2010) KQ
	CALL BUG(22)
C
C	do it again
C
2645	IF (OLDWORD1.EQ.'')GOTO 2647
	WORD1=OLDWORD1
	WORD2=OLDWORD2
C	PRINT 2650,'SET OLDWORDS for again'
	GOTO 2608
C
C	You can't do something again if you haven't done anything yet.
C
2647	CALL RSPEAK(14)
	GOTO 2012
C
2650	FORMAT('DEBUG: ',A)
C
C  Get second word for analysis.
C
2800	WORD1=WORD2
	WORD2=' '
	GOTO 2610
C
C  Gee, I don't understand.
C
3000	SPK=60
	IF(PCT(20))SPK=61
	IF(PCT(20))SPK=13
	CALL RSPEAK(SPK)
	GOTO 2600
C	Maximum verbosity.
3100	IF(VERBOSITY.EQV..TRUE.)GOTO 3101
	CALL RSPEAK(202)
	VERBOSITY=.TRUE.
	GOTO 2012
3101	CALL RSPEAK(203)
	GOTO 2012
C
C  Analyse a verb.  Remember what it was, go back for object if second word
C  unless verb is "SAY", which snarfs arbitrary second word.
C
4000	VERB=K
	SPK=ACTSPK(VERB)
	IF(WORD2.EQ.' ') GOTO 4080		! any object?
	IF(VERB.EQ.SAY) GOTO 4090		! say?
	GOTO 2800				! go analyze object
C
C  Analyse an intransitive verb (ie, no object given yet).
C
4080	GOTO(8010,8000,8000,8040,2009,8040,9070,9080,8000,8000,
     1    2011,9120,9130,8140,9150,8000,8000,8180,8000,8200,
     2    8000,9220,9230,8240,8250,8260,8270,8000,8000,8300,
     3    8310,8320)VERB
C	     TAKE DROP  SAY OPEN NOTH LOCK   ON  OFF WAVE CALM
C	     WALK KILL POUR  EAT DRNK  RUB TOSS QUIT FIND INVN
C	     FEED FILL BLST SCOR  FOO  BRF READ BREK WAKE SUSP
C	     HOUR RESU
	CALL BUG(23)
C
C  Analyse a transitive verb.
C
4090	GOTO(9010,9020,9030,9040,2009,9040,9070,9080,9090,2011,
     1    2011,9120,9130,9140,9150,9160,9170,2011,9190,9190,
     2    9210,9220,9230,2011,2011,2011,9270,9280,9290,2011,
     3    2011,2011)VERB
C	     TAKE DROP  SAY OPEN NOTH LOCK   ON  OFF WAVE CALM
C	     WALK KILL POUR  EAT DRNK  RUB TOSS QUIT FIND INVN
C	     FEED FILL BLST SCOR  FOO  BRF READ BREK WAKE SUSP
C	     HOUR RESU
	CALL BUG(24)
C
C  Analyse an object word.  See if the thing is here, whether we've got a verb
C  yet, and so on.  Object must be here unless verb is "FIND" or "INVENT(ORY)"
C  (and no new verb yet to be analysed).  Water and oil are also funny, since
C  they are never actually dropped at any location, but might be here inside
C  the bottle or as a feature of the location.
C
5000	OBJ=K
	IF(FIXED(K).NE.LOC.AND..NOT.HERE(K))GOTO 5100
5010	IF(WORD2.NE.' ')GOTO 2800
	IF(VERB.NE.0)GOTO 4090
	PRINT 5015, WORD1(1:NBLEN(WORD1))
5015	FORMAT(' What do you want to do with the ',A,'?')
	GOTO 2600
C
5100	IF(K.NE.GRATE)GOTO 5110
	IF(LOC.EQ.1.OR.LOC.EQ.4.OR.LOC.EQ.7)K=DPRSSN
	IF(LOC.GT.9.AND.LOC.LT.15)K=ENTRNC
	IF(K.NE.GRATE)GOTO 8
5110	IF(K.NE.DWARF)GOTO 5120
	DO 5112 I=1,5
	IF(DLOC(I).EQ.LOC.AND.DFLAG.GE.2)GOTO 5010
5112	CONTINUE
5120	IF((LIQ(0).EQ.K.AND.HERE(BOTTLE)).OR.K.EQ.LIQLOC(LOC))GOTO 5010
	IF(OBJ.NE.PLANT.OR..NOT.AT(PLANT2).OR.PROP(PLANT2).EQ.0)GOTO 5130
	OBJ=PLANT2
	GOTO 5010
5130	IF(OBJ.NE.KNIFE.OR.KNFLOC.NE.LOC)GOTO 5140
	KNFLOC=-1
	SPK=116
	GOTO 2011
5140	IF(OBJ.NE.ROD.OR..NOT.HERE(ROD2))GOTO 5190
	OBJ=ROD2
	GOTO 5010
5190	IF((VERB.EQ.FIND.OR.VERB.EQ.INVENT).AND.WORD2.EQ.' ')GOTO 5010
	PRINT 5199, WORD1(1:NBLEN(WORD1))
5199	FORMAT(' I don''t see any ',A,'.')
	GOTO 2012

C  Figure out the new location
C
C  Given the current location in "LOC", and a motion verb number in "K", put
C  the new location in "NEWLOC".  The current LOC is saved in "OLDLOC" in case
C  he wants to retreat.  The current OLDLOC is saved in OLDLC2, in case he
C  dies.  (If he does, NEWLOC will be limbo, and OLDLOC will be what killed
C  him, so we need OLDLC2, which is the last place he was safe.)
C
8	KK=KEY(LOC)
	NEWLOC=LOC
	IF(KK.EQ.0)CALL BUG(26)
	IF(K.EQ.NULL)GOTO 2
	IF(K.EQ.BACK)GOTO 20
	IF(K.EQ.LOOK)GOTO 30
	IF(K.EQ.CAVE)GOTO 40
	OLDLC2=OLDLOC
	OLDLOC=LOC
C
9	LL=IABS(TRAVEL(KK))
	IF(LL.EQ.1 .OR. LL.EQ.K)GOTO 10
	IF(TRAVEL(KK).LT.0)GOTO 50
	KK=KK+1
	GOTO 9
C
10	NEWLOC=TRVCON(KK)
	K=MOD(NEWLOC,OBJMAX)
	IF(NEWLOC.LE.300)GOTO 13
	IF(PROP(K).NE.NEWLOC/OBJMAX-3)GOTO 16
C
C  Try next entry in travel table
C
12	IF(TRAVEL(KK).LT.0)CALL BUG(25)
	KK=KK+1
C
C  Make sure he doesn't go through same test again
C
	IF(TRVCON(KK-1).EQ.TRVCON(KK) .AND. TRVLOC(KK-1).EQ.TRVLOC(KK))
     1  GOTO 12
	GO TO 10
C
13	IF(NEWLOC.LE.OBJMAX)GOTO 14
	IF(TOTING(K).OR.(NEWLOC.GT.200.AND.AT(K)))GOTO 16
	GOTO 12
C
14	IF(NEWLOC.NE.0.AND..NOT.PCT(NEWLOC))GOTO 12
16	NEWLOC=TRVLOC(KK)
	IF(NEWLOC.LE.300)GOTO 2
	IF(NEWLOC.LE.500)GOTO 30000
	CALL RSPEAK(NEWLOC-500)
	NEWLOC=LOC
	GOTO 2
C
C  Special motions come here.  Labelling convention: statement numbers NNNXX
C  (XX=00-99) ARE used for special case number NNN (NNN=301-500).
C
30000	NEWLOC=NEWLOC-300
	GOTO (30100,30200,30300)NEWLOC
	CALL BUG(20)
C
C  Travel 301.  Plover-Alcove passage.  Can carry only emerald.  Note: travel
C  table must include "useless" entries going through passage, which can never
C  be used for actual motion, but can be spotted by "go back".
C
30100	NEWLOC=99+100-LOC
	IF(HOLDNG.EQ.0.OR.(HOLDNG.EQ.1.AND.TOTING(EMRALD)))GOTO 2
	NEWLOC=LOC
	CALL RSPEAK(117)
	GOTO 2
C
C  Travel 302.  Plover transport.  Drop the emerald (only use special travel if
C  toting it), so he's forced to use the Plover-passage to get it out.  Having
C  dropped it, go back and pretend he wasn't carrying it after all.
C
30200	CALL DROP(EMRALD,LOC)
	GOTO 12
C
C  Travel 303.  Troll Bridge.  Must be done only as special motion so that
C  dwarves won't wander across and encounter the bear.  (They won't follow the
C  player there because that region is forbidden to the pirate.)  If
C  PROP(TROLL)=1, he's crossed since paying, so step out and block him.
C  (Standard travel entries check for PROP(TROLL)=0.)  Special stuff for bear.
C
30300	IF(PROP(TROLL).NE.1)GOTO 30310
	CALL PSPEAK(TROLL,1)
	PROP(TROLL)=0
	CALL MOVE(TROLL2,0)
	CALL MOVE(TROLL2+OBJMAX,0)
	CALL MOVE(TROLL,PLAC(TROLL))
	CALL MOVE(TROLL+OBJMAX,FIXD(TROLL))
	CALL JUGGLE(CHASM)
	NEWLOC=LOC
	GOTO 2
C
30310	NEWLOC=PLAC(TROLL)+FIXD(TROLL)-LOC
	IF(PROP(TROLL).EQ.0)PROP(TROLL)=1
	IF(.NOT.TOTING(BEAR))GOTO 2
	CALL RSPEAK(162)
	PROP(CHASM)=1
	PROP(TROLL)=2
	CALL DROP(BEAR,NEWLOC)
	FIXED(BEAR)=-1
	PROP(BEAR)=3
	IF(PROP(SPICES).LT.0)TALLY2=TALLY2+1
	OLDLC2=NEWLOC
	GOTO 99
C
C  End of specials.
C
C  Handle "GO BACK".  Look for verb which goes from LOC to OLDLOC, or to OLDLC2
C  if OLDLOC HAS FORCED-MOTION.  K2 saves entry -> forced LOC -> previous LOC.
C
20	K=OLDLOC
	IF(FORCED(K))K=OLDLC2
	OLDLC2=OLDLOC
	OLDLOC=LOC
	K2=0
	IF(K.NE.LOC)GOTO 21
	CALL RSPEAK(91)
	GOTO 2
C
21	LL=TRVLOC(KK)
	IF(LL.EQ.K)GOTO 25
	IF(LL.GT.300)GOTO 22
	J=KEY(LL)
	IF(FORCED(LL).AND.TRVLOC(KK).EQ.K)K2=KK
22	IF(TRAVEL(KK).LT.0)GOTO 23
	KK=KK+1
	GOTO 21
C
23	KK=K2
	IF(KK.NE.0)GOTO 25
	CALL RSPEAK(140)
	GOTO 2
C
25	K=IABS(TRAVEL(KK))
	KK=KEY(LOC)
	GOTO 9
C
C  Look.  Can't give more detail.  Pretend it wasn't dark (though it may "now"
C  be dark) so he won't fall into a pit while staring into the gloom.
C
30	IF(DETAIL.LT.3)CALL RSPEAK(15)
	DETAIL=DETAIL+1
	WZDARK=.FALSE.
	ABB(LOC)=0
	GOTO 2
C
C  Cave.  Different messages depending on whether above ground.
C
40	IF(LOC.LT.8)CALL RSPEAK(57)
	IF(LOC.GE.8)CALL RSPEAK(58)
	GOTO 2
C
C  Non-applicable motion.  Various messages depending on word given.
C
50	SPK=12
	IF(K.GE.43.AND.K.LE.50)SPK=9
	IF(K.EQ.29.OR.K.EQ.30)SPK=9
	IF(K.EQ.7.OR.K.EQ.36.OR.K.EQ.37)SPK=10
	IF(K.EQ.11.OR.K.EQ.19)SPK=11
	IF(VERB.EQ.FIND.OR.VERB.EQ.INVENT)SPK=59
	IF(K.EQ.62.OR.K.EQ.65)SPK=42
	IF(K.EQ.17)SPK=80
	CALL RSPEAK(SPK)
	GOTO 2

C  "You're dead, Jim."
C
C  If the current LOC is zero, it means the clown got himself killed.  We'll
C  allow this MAXDIE times.  MAXDIE is automatically set based on the number of
C  snide messages available.  Each death results in a message (81, 83, etc.)
C  which offers reincarnation; if accepted, this results in message 82, 84,
C  etc.  The last time, if he wants another chance, he gets a snide remark as
C  we exit.  When reincarnated, all objects being carried get dropped at OLDLC2
C  (presumably the last place prior to being killed) without change of PROPS.
C  The loop runs backwards to assure that the bird is dropped before the cage.
C  (This kluge could be changed once we're sure all references to bird and cage
C  are done by keywords.)  The lamp is a special case (it wouldn't do to leave
C  it in the cave).  It is turned off and left outside the building (only if he
C  was carrying it, of course).  He himself is left inside the building (and
C  heaven help him if he tries to XYZZY back into the cave without the lamp!).
C  OLDLOC is zapped so he can't just "RETREAT".
C
C  The easiest way to get killed is to fall into a pit in pitch darkness.
C
90	CALL RSPEAK(23)
	OLDLC2=LOC
C
C  Okay, he's dead.  Let's get on with it.
C
99	IF(CLOSNG)GOTO 95
	YEA=YES(81+NUMDIE*2,82+NUMDIE*2,54)
	NUMDIE=NUMDIE+1
	IF(NUMDIE.EQ.MAXDIE.OR..NOT.YEA)GOTO 20000
	PLACE(WATER)=0
	PLACE(OIL)=0
	IF(TOTING(LAMP))PROP(LAMP)=0
	DO 98 J=1,OBJMAX
	I=OBJMAX+1-J
	IF(.NOT.TOTING(I))GOTO 98
	K=OLDLC2
	IF(I.EQ.LAMP)K=1
	CALL DROP(I,K)
98	CONTINUE
	LOC=3
	OLDLOC=LOC
	GOTO 2000
C
C  He died during closing time.  No resurrection.  Tally up a death and exit.
C
95	CALL RSPEAK(131)
	NUMDIE=NUMDIE+1
	GOTO 20000

C  Routines for performing the various action verbs
C
C  Statement numbers in this section are 8000 for intransitive verbs, 9000 for
C  transitive, plus ten times the verb number.  Many intransitive verbs use the
C  transitive code, and some verbs use code for other verbs, as noted below.
C
C  Random intransitive verbs come here.  Clear OBJ just in case (see "ATTACK").
C
8000	PRINT 8002, WORD1(1:NBLEN(WORD1))
8002	FORMAT(' I don''t understand "',A,'".')
	OBJ=0
	GOTO 2600
C
C  Carry, no object given yet.  Ok if only one object present.
C
8010	IF(ATLOC(LOC).EQ.0.OR.LINK(ATLOC(LOC)).NE.0)GOTO 8000
	DO 8012 I=1,5
	IF(DLOC(I).EQ.LOC.AND.DFLAG.GE.2)GOTO 8000
8012	CONTINUE
	OBJ=ATLOC(LOC)
C
C  Carry an object.  Special cases for bird and cage (if bird in cage, can't
C  take one without the other.  Liquids also special, since they depend on
C  status of bottle.  Also various side effects, etc.
C
9010	IF(TOTING(OBJ))GOTO 2011
	SPK=25
	IF(OBJ.EQ.PLANT.AND.PROP(PLANT).LE.0)SPK=115
	IF(OBJ.EQ.BEAR.AND.PROP(BEAR).EQ.1)SPK=169
	IF(OBJ.EQ.CHAIN.AND.PROP(BEAR).NE.0)SPK=170
	IF(FIXED(OBJ).NE.0)GOTO 2011
	IF(OBJ.NE.WATER.AND.OBJ.NE.OIL)GOTO 9017
	IF(HERE(BOTTLE).AND.LIQ(0).EQ.OBJ)GOTO 9018
	OBJ=BOTTLE
	IF(TOTING(BOTTLE).AND.PROP(BOTTLE).EQ.1)GOTO 9220
	IF(PROP(BOTTLE).NE.1)SPK=105
	IF(.NOT.TOTING(BOTTLE))SPK=104
	GOTO 2011
9018	OBJ=BOTTLE
9017	IF(HOLDNG.LT.7)GOTO 9016
	CALL RSPEAK(92)
	GOTO 2012
9016	IF(OBJ.NE.BIRD)GOTO 9014
	IF(PROP(BIRD).NE.0)GOTO 9014
	IF(.NOT.TOTING(ROD))GOTO 9013
	CALL RSPEAK(26)
	GOTO 2012
9013	IF(TOTING(CAGE))GOTO 9015
	CALL RSPEAK(27)
	GOTO 2012
9015	PROP(BIRD)=1
9014	IF((OBJ.EQ.BIRD.OR.OBJ.EQ.CAGE).AND.PROP(BIRD).NE.0)
     1  CALL CARRY(BIRD+CAGE-OBJ,LOC)
	CALL CARRY(OBJ,LOC)
	K=LIQ(0)
	IF(OBJ.EQ.BOTTLE.AND.K.NE.0)PLACE(K)=-1
	GOTO 2009
C
C  Discard object.  "THROW" also comes here for most objects.  Special cases for
C  bird (might attack snake or dragon) and cage (might contain bird) and vase.
C  Drop coins at vending machine for extra batteries.
C
9020	IF(TOTING(ROD2).AND.OBJ.EQ.ROD.AND..NOT.TOTING(ROD))OBJ=ROD2
	IF(.NOT.TOTING(OBJ))GOTO 2011
	IF(OBJ.NE.BIRD.OR..NOT.HERE(SNAKE))GOTO 9024
	CALL RSPEAK(30)
	IF(CLOSED)GOTO 19000
	CALL DSTROY(SNAKE)
C
C  Set prop for use by travel options
C
	PROP(SNAKE)=1
9021	K=LIQ(0)
	IF(K.EQ.OBJ)OBJ=BOTTLE
	IF(OBJ.EQ.BOTTLE.AND.K.NE.0)PLACE(K)=0
	IF(OBJ.EQ.CAGE.AND.PROP(BIRD).NE.0)CALL DROP(BIRD,LOC)
	IF(OBJ.EQ.BIRD)PROP(BIRD)=0
	CALL DROP(OBJ,LOC)
	GOTO 2012
C
9024	IF(OBJ.NE.COINS.OR..NOT.HERE(VEND))GOTO 9025
	CALL DSTROY(COINS)
	CALL DROP(BATTER,LOC)
	CALL PSPEAK(BATTER,0)
	GOTO 2012
C
9025	IF(OBJ.NE.BIRD.OR..NOT.AT(DRAGON).OR.PROP(DRAGON).NE.0)GOTO 9026
	CALL RSPEAK(154)
	CALL DSTROY(BIRD)
	PROP(BIRD)=0
	IF(PLACE(SNAKE).EQ.PLAC(SNAKE))TALLY2=TALLY2+1
	GOTO 2012
C
9026	IF(OBJ.NE.BEAR.OR..NOT.AT(TROLL))GOTO 9027
	CALL RSPEAK(163)
	CALL MOVE(TROLL,0)
	CALL MOVE(TROLL+OBJMAX,0)
	CALL MOVE(TROLL2,PLAC(TROLL))
	CALL MOVE(TROLL2+OBJMAX,FIXD(TROLL))
	CALL JUGGLE(CHASM)
	PROP(TROLL)=2
	GOTO 9021
C
9027	IF(OBJ.EQ.VASE.AND.LOC.NE.PLAC(PILLOW))GOTO 9028
	CALL RSPEAK(54)
	GOTO 9021
C
9028	PROP(VASE)=2
	IF(AT(PILLOW))PROP(VASE)=0
	CALL PSPEAK(VASE,PROP(VASE)+1)
	IF(PROP(VASE).NE.0)FIXED(VASE)=-1
	GOTO 9021
C
C  SAY.  Echo WORD2 (or WORD1 if no WORD2 (SAY what?, etc.).)
C  Magic words override.
C
9030	IF(WORD2.EQ.' ') GOTO 9031
	WORD1=WORD2
9031	CALL VOCAB(WORD1,-1,I)
	IF(I.EQ.62.OR.I.EQ.65.OR.I.EQ.71.OR.I.EQ.2025)GOTO 9035
	PRINT 9032, WORD1(1:NBLEN(WORD1))
9032	FORMAT(' Okay, "',A,'".')
	GOTO 2012
C
9035	WORD2=' '
	OBJ=0
	GOTO 2630
C
C  LOCK, UNLOCK, no object given.  Assume various things if present.
C
8040	SPK=28
	IF(HERE(CLAM))OBJ=CLAM
	IF(HERE(OYSTER))OBJ=OYSTER
	IF(AT(DOOR))OBJ=DOOR
	IF(AT(GRATE))OBJ=GRATE
	IF(OBJ.NE.0.AND.HERE(CHAIN))GOTO 8000
	IF(HERE(CHAIN))OBJ=CHAIN
	IF(OBJ.EQ.0)GOTO 2011
C
C  LOCK, UNLOCK object.  Special stuff for opening clam/oyster and for chain.
C
9040	IF(OBJ.EQ.CLAM.OR.OBJ.EQ.OYSTER)GOTO 9046
	IF(OBJ.EQ.DOOR)SPK=111
	IF(OBJ.EQ.DOOR.AND.PROP(DOOR).EQ.1)SPK=54
	IF(OBJ.EQ.CAGE)SPK=32
	IF(OBJ.EQ.KEYS)SPK=55
	IF(OBJ.EQ.GRATE.OR.OBJ.EQ.CHAIN)SPK=31
	IF(SPK.NE.31.OR..NOT.HERE(KEYS))GOTO 2011
	IF(OBJ.EQ.CHAIN)GOTO 9048
	IF(.NOT.CLOSNG)GOTO 9043
	K=130
	IF(.NOT.PANIC)CLOCK2=15
	PANIC=.TRUE.
	GOTO 2010
C
9043	K=34+PROP(GRATE)
	PROP(GRATE)=1
	IF(VERB.EQ.LOCK)PROP(GRATE)=0
	K=K+2*PROP(GRATE)
	GOTO 2010
C
C  Clam/oyster.
C
9046	K=0
	IF(OBJ.EQ.OYSTER)K=1
	SPK=124+K
	IF(TOTING(OBJ))SPK=120+K
	IF(.NOT.TOTING(TRIDNT))SPK=122+K
	IF(VERB.EQ.LOCK)SPK=61
	IF(SPK.NE.124)GOTO 2011
	CALL DSTROY(CLAM)
	CALL DROP(OYSTER,LOC)
	CALL DROP(PEARL,105)
	GOTO 2011
C
C  Chain.
C
9048	IF(VERB.EQ.LOCK)GOTO 9049
	SPK=171
	IF(PROP(BEAR).EQ.0)SPK=41
	IF(PROP(CHAIN).EQ.0)SPK=37
	IF(SPK.NE.171)GOTO 2011
	PROP(CHAIN)=0
	FIXED(CHAIN)=0
	IF(PROP(BEAR).NE.3)PROP(BEAR)=2
	FIXED(BEAR)=2-PROP(BEAR)
	GOTO 2011
C
9049	SPK=172
	IF(PROP(CHAIN).NE.0)SPK=34
	IF(LOC.NE.PLAC(CHAIN))SPK=173
	IF(SPK.NE.172)GOTO 2011
	PROP(CHAIN)=2
	IF(TOTING(CHAIN))CALL DROP(CHAIN,LOC)
	FIXED(CHAIN)=-1
	GOTO 2011
C
C  Light lamp
C
9070	IF(.NOT.HERE(LAMP))GOTO 2011
	SPK=184
	IF(LIMIT.LT.0)GOTO 2011
	PROP(LAMP)=1
	CALL RSPEAK(39)
	IF(WZDARK)GOTO 2000
	GOTO 2012
C
C  Lamp off
C
9080	IF(.NOT.HERE(LAMP))GOTO 2011
	PROP(LAMP)=0
	CALL RSPEAK(40)
	IF(DARK(0))CALL RSPEAK(16)
	GOTO 2012
C
C  Wave.  No effect unless waving rod at fissure.
C
9090	IF((.NOT.TOTING(OBJ)).AND.(OBJ.NE.ROD.OR..NOT.TOTING(ROD2)))
     1  SPK=29
	IF(OBJ.NE.ROD.OR..NOT.AT(FISSUR).OR..NOT.TOTING(OBJ)
     1  .OR.CLOSNG)GOTO 2011
	PROP(FISSUR)=1-PROP(FISSUR)
	CALL PSPEAK(FISSUR,2-PROP(FISSUR))
	GOTO 2012
C
C  Attack.  Assume target if unambiguous.  "THROW" also links here.  Attackable
C  objects fall into two categories: enemies (snake, dwarf, etc.)  and others
C  (bird, clam).  Ambiguous if two enemies, or if no enemies but two others.
C
9120	DO 9121 I=1,5
	IF(DLOC(I).EQ.LOC.AND.DFLAG.GE.2)GOTO 9122
9121	CONTINUE
	I=0
9122	IF(OBJ.NE.0)GOTO 9124
	IF(I.NE.0)OBJ=DWARF
	IF(HERE(SNAKE))OBJ=OBJ*100+SNAKE
	IF(AT(DRAGON).AND.PROP(DRAGON).EQ.0)OBJ=OBJ*100+DRAGON
	IF(AT(TROLL))OBJ=OBJ*100+TROLL
	IF(HERE(BEAR).AND.PROP(BEAR).EQ.0)OBJ=OBJ*100+BEAR
	IF(OBJ.GT.100)GOTO 8000
	IF(OBJ.NE.0)GOTO 9124
C
C  Can't attack bird by throwing axe.
C
	IF(HERE(BIRD).AND.VERB.NE.THROW)OBJ=BIRD
C
C  Clam and oyster both treated as clam for intransitive case; no harm done.
C
	IF(HERE(CLAM).OR.HERE(OYSTER))OBJ=100*OBJ+CLAM
	IF(OBJ.GT.100)GOTO 8000
9124	IF(OBJ.NE.BIRD)GOTO 9125
	SPK=137
	IF(CLOSED)GOTO 2011
	CALL DSTROY(BIRD)
	PROP(BIRD)=0
	IF(PLACE(SNAKE).EQ.PLAC(SNAKE))TALLY2=TALLY2+1
	SPK=45
9125	IF(OBJ.EQ.0)SPK=44
	IF(OBJ.EQ.CLAM.OR.OBJ.EQ.OYSTER)SPK=150
	IF(OBJ.EQ.SNAKE)SPK=46
	IF(OBJ.EQ.DWARF)SPK=49
	IF(OBJ.EQ.DWARF.AND.CLOSED)GOTO 19000
	IF(OBJ.EQ.DRAGON)SPK=167
	IF(OBJ.EQ.TROLL)SPK=157
	IF(OBJ.EQ.BEAR)SPK=165+(PROP(BEAR)+1)/2
	IF(OBJ.NE.DRAGON.OR.PROP(DRAGON).NE.0)GOTO 2011
C
C  Fun stuff for dragon.  If he insists on attacking it, win!  Set prop to dead,
C  move dragon to central LOC (still fixed), move rug there (not fixed), and
C  move him there, too.  Then do a null motion to get new description.
C
	CALL RSPEAK(49)
	VERB=0
	OBJ=0
	CALL GETIN(WORD1,WORD2)
	IF(WORD1.NE.'Y' .AND. WORD1.NE.'YE' .AND. WORD1.NE.'YES')
     1    GOTO 2608
	CALL PSPEAK(DRAGON,1)
	PROP(DRAGON)=2
	PROP(RUG)=0
	K=(PLAC(DRAGON)+FIXD(DRAGON))/2
	CALL MOVE(DRAGON+OBJMAX,-1)
	CALL MOVE(RUG+OBJMAX,0)
	CALL MOVE(DRAGON,K)
	CALL MOVE(RUG,K)
	DO 9126 OBJ=1,OBJMAX
	IF(PLACE(OBJ).EQ.PLAC(DRAGON).OR.PLACE(OBJ).EQ.FIXD(DRAGON))
     1  CALL MOVE(OBJ,K)
9126	CONTINUE
	LOC=K
	K=NULL
	GOTO 8
C
C  POUR.  If no object, or object is bottle, assume contents of bottle.
C  Special tests for pouring water or oil on plant or rusty door.
C
9130	IF(OBJ.EQ.BOTTLE.OR.OBJ.EQ.0)OBJ=LIQ(0)
	IF(OBJ.EQ.0)GOTO 8000
	IF(.NOT.TOTING(OBJ))GOTO 2011
	SPK=78
	IF(OBJ.NE.OIL.AND.OBJ.NE.WATER)GOTO 2011
	PROP(BOTTLE)=1
	PLACE(OBJ)=0
	SPK=77
	IF(.NOT.(AT(PLANT).OR.AT(DOOR)))GOTO 2011
C
	IF(AT(DOOR))GOTO 9132
	SPK=112
	IF(OBJ.NE.WATER)GOTO 2011
	CALL PSPEAK(PLANT,PROP(PLANT)+1)
	PROP(PLANT)=MOD(PROP(PLANT)+2,6)
	PROP(PLANT2)=PROP(PLANT)/2
	K=NULL
	GOTO 8
C
9132	PROP(DOOR)=0
	IF(OBJ.EQ.OIL)PROP(DOOR)=1
	SPK=113+PROP(DOOR)
	GOTO 2011
C
C  EAT.  Intransitive: assume food if present, else ask what.  Transitive: food
C  ok, some things lose appetite, rest are ridiculous.
C
8140	IF(.NOT.HERE(FOOD))GOTO 8000
8142	CALL DSTROY(FOOD)
	SPK=72
	GOTO 2011
C
9140	IF(OBJ.EQ.FOOD)GOTO 8142
	IF(OBJ.EQ.BIRD.OR.OBJ.EQ.SNAKE.OR.OBJ.EQ.CLAM.OR.OBJ.EQ.OYSTER
     1  .OR.OBJ.EQ.DWARF.OR.OBJ.EQ.DRAGON.OR.OBJ.EQ.TROLL
     2  .OR.OBJ.EQ.BEAR)SPK=71
	GOTO 2011
C
C  DRINK.  If no object, assume water and look for it here.  If water is in
C  the bottle, drink that, else must be at a water LOC, so drink stream.
C
9150	IF(OBJ.EQ.0.AND.LIQLOC(LOC).NE.WATER.AND.(LIQ(0).NE.WATER
     1  .OR..NOT.HERE(BOTTLE)))GOTO 8000
	IF(OBJ.NE.0.AND.OBJ.NE.WATER)SPK=110
	IF(SPK.EQ.110.OR.LIQ(0).NE.WATER.OR..NOT.HERE(BOTTLE))GOTO 2011
	PROP(BOTTLE)=1
	PLACE(WATER)=0
	SPK=74
	GOTO 2011
C
C  RUB.  Yields various snide remarks.
C
9160	IF(OBJ.NE.LAMP)SPK=76
	GOTO 2011
C
C  THROW.  Same as discard unless axe.  Then same as attack except ignore bird,
C  and if dwarf is present then one might be killed.  (Only way to do so!)
C  Axe also special for dragon, bear, and troll.  Treasures special for troll.
C
9170	IF(TOTING(ROD2).AND.OBJ.EQ.ROD.AND..NOT.TOTING(ROD))OBJ=ROD2
	IF(.NOT.TOTING(OBJ))GOTO 2011
	IF(OBJ.GE.50.AND.OBJ.LE.MAXTRS.AND.AT(TROLL))GOTO 9178
	IF(OBJ.EQ.FOOD.AND.HERE(BEAR))GOTO 9177
	IF(OBJ.NE.AXE)GOTO 9020
	DO 9171 I=1,5
C
C  Needn't check dflag if axe is here.
C
	IF(DLOC(I).EQ.LOC)GOTO 9172
9171	CONTINUE
	SPK=152
	IF(AT(DRAGON).AND.PROP(DRAGON).EQ.0)GOTO 9175
	SPK=158
	IF(AT(TROLL))GOTO 9175
	IF(HERE(BEAR).AND.PROP(BEAR).EQ.0)GOTO 9176
	OBJ=0
	GOTO 9120
C
9172	SPK=48
	IF(RND(3).EQ.0)GOTO 9175
	DSEEN(I)=.FALSE.
	DLOC(I)=0
	SPK=47
	DKILL=DKILL+1
	IF(DKILL.EQ.1)SPK=149
9175	CALL RSPEAK(SPK)
	CALL DROP(AXE,LOC)
	K=NULL
	GOTO 8
C
C  This'll teach him to throw the axe at the bear!
C
9176	SPK=164
	CALL DROP(AXE,LOC)
	FIXED(AXE)=-1
	PROP(AXE)=1
	CALL JUGGLE(BEAR)
	GOTO 2011
C
C  But throwing food is another story.
C
9177	OBJ=BEAR
	GOTO 9210
C
C  Snarf a treasure for the troll.
C
9178	SPK=159
	CALL DROP(OBJ,0)
	CALL MOVE(TROLL,0)
	CALL MOVE(TROLL+OBJMAX,0)
	CALL DROP(TROLL2,PLAC(TROLL))
	CALL DROP(TROLL2+OBJMAX,FIXD(TROLL))
	CALL JUGGLE(CHASM)
	GOTO 2011
C
C  QUIT.  Intransitive only.  Verify intent and exit if that's what he wants.
C
8180	GAVEUP=YES(22,54,54)
8185	IF(GAVEUP)GOTO 20000
	GOTO 2012
C
C  FIND.  Might be carrying it, or it might be here.  Else give caveat.
C
9190	IF(AT(OBJ).OR.(LIQ(0).EQ.OBJ.AND.AT(BOTTLE))
     1  .OR.K.EQ.LIQLOC(LOC))SPK=94
	DO 9192 I=1,5
9192	IF(DLOC(I).EQ.LOC.AND.DFLAG.GE.2.AND.OBJ.EQ.DWARF)SPK=94
	IF(CLOSED)SPK=138
	IF(TOTING(OBJ))SPK=24
	GOTO 2011
C
C  INVENTORY.  If object, treat same as FIND.  Else report on current burden.
C
8200	SPK=98
	DO 8201 I=1,OBJMAX
	IF(I.EQ.BEAR.OR..NOT.TOTING(I))GOTO 8201
	IF(SPK.EQ.98)CALL RSPEAK(99)
	CALL PSPEAK(I,-1)
	SPK=0
8201	CONTINUE
	IF(TOTING(BEAR))SPK=141
	GOTO 2011
C
C  FEED.  If bird, no seed.  Snake, dragon, troll: quip.  If dwarf, make him
C  mad.  Bear, special.
C
9210	IF(OBJ.NE.BIRD)GOTO 9212
	SPK=100
	GOTO 2011
C
9212	IF(OBJ.NE.SNAKE.AND.OBJ.NE.DRAGON.AND.OBJ.NE.TROLL)GOTO 9213
	SPK=102
	IF(OBJ.EQ.DRAGON.AND.PROP(DRAGON).NE.0)SPK=110
	IF(OBJ.EQ.TROLL)SPK=182
	IF(OBJ.NE.SNAKE.OR.CLOSED.OR..NOT.HERE(BIRD))GOTO 2011
	SPK=101
	CALL DSTROY(BIRD)
	PROP(BIRD)=0
	TALLY2=TALLY2+1
	GOTO 2011
C
9213	IF(OBJ.NE.DWARF)GOTO 9214
	IF(.NOT.HERE(FOOD))GOTO 2011
	SPK=103
	DFLAG=DFLAG+1
	GOTO 2011
C
9214	IF(OBJ.NE.BEAR)GOTO 9215
	IF(PROP(BEAR).EQ.0)SPK=102
	IF(PROP(BEAR).EQ.3)SPK=110
	IF(.NOT.HERE(FOOD))GOTO 2011
	CALL DSTROY(FOOD)
	PROP(BEAR)=1
	FIXED(AXE)=0
	PROP(AXE)=0
	SPK=168
	GOTO 2011
C
9215	SPK=14
	GOTO 2011
C
C  FILL.  Bottle must be empty, and some liquid available.  (Vase is nasty.)
C
9220	IF(OBJ.EQ.VASE)GOTO 9222
	IF(OBJ.NE.0.AND.OBJ.NE.BOTTLE)GOTO 2011
	IF(OBJ.EQ.0.AND..NOT.HERE(BOTTLE))GOTO 8000
	SPK=107
	IF(LIQLOC(LOC).EQ.0)SPK=106
	IF(LIQ(0).NE.0)SPK=105
	IF(SPK.NE.107)GOTO 2011
	PROP(BOTTLE)=MOD(COND(LOC),4)/2*2
	K=LIQ(0)
	IF(TOTING(BOTTLE))PLACE(K)=-1
	IF(K.EQ.OIL)SPK=108
	GOTO 2011
C
9222	SPK=29
	IF(LIQLOC(LOC).EQ.0)SPK=144
	IF(LIQLOC(LOC).EQ.0.OR..NOT.TOTING(VASE))GOTO 2011
	CALL RSPEAK(145)
	PROP(VASE)=2
	FIXED(VASE)=-1
	GOTO 9024
C
C  BLAST.  No effect unless you've got dynamite, which is a neat trick!
C
9230	IF(PROP(ROD2).LT.0.OR..NOT.CLOSED)GOTO 2011
	BONUS=133
	IF(LOC.EQ.115)BONUS=134
	IF(HERE(ROD2))BONUS=135
	CALL RSPEAK(BONUS)
	GOTO 20000
C
C  SCORE.  Go to scoring section, which will return to 8241 if SCORNG is true.
C
8240	SCORNG=.TRUE.
	GOTO 20000
C
8241	SCORNG=.FALSE.
	PRINT 8243,SCORE,MXSCOR
8243	FORMAT(' If you were to quit now, you would score',I4
     1  ,' out of a possible',I4,'.')
	GAVEUP=YES(143,54,54)
	GOTO 8185
C
C  FEE FIE FOE FOO (and FUM).  Advance to next state if given in proper order.
C  Look up WORD1 in section 3 of VOCAB to determine which word we've got.  Last
C  word zips the eggs back to the Giant Room (unless already there).
C
8250	CALL VOCAB(WORD1,3,K)
	SPK=42
	IF(FOOBAR.EQ.1-K)GOTO 8252
	IF(FOOBAR.NE.0)SPK=151
	GOTO 2011
C
8252	FOOBAR=K
	IF(K.NE.4)GOTO 2009
	FOOBAR=0
	IF(PLACE(EGGS).EQ.PLAC(EGGS)
     1  .OR.(TOTING(EGGS).AND.LOC.EQ.PLAC(EGGS)))GOTO 2011
C
C  Bring back troll if we steal the eggs back from him before crossing.
C
	IF(PLACE(EGGS).EQ.0.AND.PLACE(TROLL).EQ.0.AND.PROP(TROLL).EQ.0)
     1  PROP(TROLL)=1
	K=2
	IF(HERE(EGGS))K=1
	IF(LOC.EQ.PLAC(EGGS))K=0
	CALL MOVE(EGGS,PLAC(EGGS))
	CALL PSPEAK(EGGS,K)
	GOTO 2012
C
C  BRIEF.  Intransitive only.  Suppress long descriptions after first time.
C
8260	SPK=156
	ABBNUM=10000
	DETAIL=3
	VERBOSITY=.FALSE.
	GOTO 2011
C
C  READ.  Magazines in dwarvish, message we've seen, and . . . oyster?
C
8270	IF(HERE(MAGZIN))OBJ=MAGZIN
	IF(HERE(TABLET))OBJ=OBJ*100+TABLET
	IF(HERE(MESSAG))OBJ=OBJ*100+MESSAG
	IF(CLOSED.AND.TOTING(OYSTER))OBJ=OYSTER
	IF(OBJ.GT.100.OR.OBJ.EQ.0.OR.DARK(0))GOTO 8000
C
9270	IF(DARK(0))GOTO 5190
	IF(OBJ.EQ.MAGZIN)SPK=190
	IF(OBJ.EQ.TABLET)SPK=196
	IF(OBJ.EQ.MESSAG)SPK=191
	IF(OBJ.EQ.OYSTER.AND.HINTED(2).AND.TOTING(OYSTER))SPK=194
	IF(OBJ.NE.OYSTER.OR.HINTED(2).OR..NOT.TOTING(OYSTER)
     1  .OR..NOT.CLOSED)GOTO 2011
	HINTED(2)=YES(192,193,54)
	GOTO 2012
C
C  BREAK.  Only works for mirror in Repository and, of course, the vase.
C
9280	IF(OBJ.EQ.MIRROR)SPK=148
	IF(OBJ.EQ.VASE.AND.PROP(VASE).EQ.0)GOTO 9282
	IF(OBJ.NE.MIRROR.OR..NOT.CLOSED)GOTO 2011
	CALL RSPEAK(197)
	GOTO 19000
C
9282	SPK=198
	IF(TOTING(VASE))CALL DROP(VASE,LOC)
	PROP(VASE)=2
	FIXED(VASE)=-1
	GOTO 2011
C
C  WAKE.  Only use is to disturb the dwarves.
C
9290	IF(OBJ.NE.DWARF.OR..NOT.CLOSED)GOTO 2011
	CALL RSPEAK(199)
	GOTO 19000
C
C  SUSPEND.  Save the world.
C
8300	CALL SAVEGM(I)
	GO TO 2012
C
C  HOURS.  Just a joke.
C
8310	CALL RSPEAK(201)
	GO TO 2012
C
C  RESUME.  Restore the world.
C
8320	CALL RSTRGM(I)
	GO TO 2012
C  Cave closing and scoring
C
C  These sections handle the closing of the cave.  The cave closes "CLOCK1"
C  turns after the last treasure has been located (including the pirate's
C  chest, which may of course never show up).  Note that the treasures need not
C  have been taken yet, just located.  Hence CLOCK1 must be large enough to get
C  out of the cave (it only ticks while inside the cave).  When it hits zero,
C  we branch to 10000 to start closing the cave, and then sit back and wait for
C  him to try to get out.  If he doesn't within CLOCK2 turns, we close the
C  cave; if he does try, we assume he panics, and give him a few additional
C  turns to get frantic before we close.  When CLOCK2 hits zero, we branch to
C  11000 to transport him into the final puzzle.  Note that the puzzle depends
C  upon all sorts of random things.  For instance, there must be no water or
C  oil, since there are beanstalks which we don't want to be able to water,
C  since the code can't handle it.  Also, we can have no keys, since there is a
C  grate (having moved the fixed object!) there separating him from all the
C  treasures.  Most of these problems arise from the use of negative prop
C  numbers to suppress the object descriptions until he's actually moved the
C  objects.
C
C  When the first warning comes, we lock the grate, destroy the bridge, kill
C  all the dwarves (and the pirate), remove the troll and bear (unless dead),
C  and set "CLOSNG" to true.  Leave the dragon; too much trouble to move it.
C  from now until CLOCK2 runs out, he cannot unlock the grate, move to any
C  location outside the cave (LOC<9), or create the bridge.  Nor can he be
C  resurrected if he dies.  Note that the snake is already gone, since he got
C  to the treasure accessible only via the Hall of the Mt. King.  Also, he's
C  been in Giant Room (to get eggs), so we can refer to it.  Also also, he's
C  gotten the pearl, so we know the bivalve is an oyster.  *And*, the dwarves
C  must have been activated, since we've found chest.
C
10000	PROP(GRATE)=0
	PROP(FISSUR)=0
	DO 10010 I=1,6
	DSEEN(I)=.FALSE.
10010	DLOC(I)=0
	CALL MOVE(TROLL,0)
	CALL MOVE(TROLL+OBJMAX,0)
	CALL MOVE(TROLL2,PLAC(TROLL))
	CALL MOVE(TROLL2+OBJMAX,FIXD(TROLL))
	CALL JUGGLE(CHASM)
	IF(PROP(BEAR).NE.3)CALL DSTROY(BEAR)
	PROP(CHAIN)=0
	FIXED(CHAIN)=0
	PROP(AXE)=0
	FIXED(AXE)=0
	CALL RSPEAK(129)
	CLOCK1=-1
	CLOSNG=.TRUE.
	GOTO 19999
C
C  Once he's panicked, and CLOCK2 has run out, we come here to set up the
C  Storage Room.  The room has two locs, hardwired as 115 (NE) and 116 (SW).
C  At the NE end, we place empty bottles, a nursery of plants, a bed of
C  oysters, a pile of lamps, rods with stars, sleeping dwarves, and him.  At
C  the SW end we place grate over treasures, snake pit, covey of caged birds,
C  more rods, and pillows.  A mirror stretches across one wall.  Many of the
C  objects come from known locations and/or states (e.g. the snake is known to
C  have been destroyed and needn't be carried away from its old "PLACE"),
C  making the various objects be handled differently.  We also drop all other
C  objects he might be carrying (lest he have some which could cause trouble,
C  such as the keys).  We describe the flash of light and trundle back.
C
11000	PROP(BOTTLE)=PUT(BOTTLE,115,1)
	PROP(PLANT)=PUT(PLANT,115,0)
	PROP(OYSTER)=PUT(OYSTER,115,0)
	PROP(LAMP)=PUT(LAMP,115,0)
	PROP(ROD)=PUT(ROD,115,0)
	PROP(DWARF)=PUT(DWARF,115,0)
	LOC=115
	OLDLOC=115
	NEWLOC=115
C
C  Leave the grate with normal (non-negative property).
C
	I=PUT(GRATE,116,0)
	PROP(SNAKE)=PUT(SNAKE,116,1)
	PROP(BIRD)=PUT(BIRD,116,1)
	PROP(CAGE)=PUT(CAGE,116,0)
	PROP(ROD2)=PUT(ROD2,116,0)
	PROP(PILLOW)=PUT(PILLOW,116,0)
C
	PROP(MIRROR)=PUT(MIRROR,115,0)
	FIXED(MIRROR)=116
C
	DO 11010 I=1,OBJMAX
11010	IF(TOTING(I))CALL DSTROY(I)
C
	CALL RSPEAK(132)
	CLOSED=.TRUE.
	GOTO 2
C
C  Another way we can force an end to things is by having the lamp give out.
C  When it gets close, we come here to warn him.  We go to 12000 if the lamp
C  and fresh batteries are here, in which case we replace the batteries and
C  continue.  12200 is for other cases of lamp dying.  12400 is when it goes
C  out, and 12600 is if he's wandered outside and the lamp is used up, in which
C  case we force him to give up.
C
12000	CALL RSPEAK(188)
	PROP(BATTER)=1
	IF(TOTING(BATTER))CALL DROP(BATTER,LOC)
	LIMIT=LIMIT+2500
	LMWARN=.FALSE.
	GOTO 19999
C
12200	IF(LMWARN.OR..NOT.HERE(LAMP))GOTO 19999
	LMWARN=.TRUE.
	SPK=187
	IF(PLACE(BATTER).EQ.0)SPK=183
	IF(PROP(BATTER).EQ.1)SPK=189
	CALL RSPEAK(SPK)
	GOTO 19999
C
12400	LIMIT=-1
	PROP(LAMP)=0
	IF(HERE(LAMP))CALL RSPEAK(184)
	GOTO 19999
C
12600	CALL RSPEAK(185)
	GAVEUP=.TRUE.
	GOTO 20000
C
C
C  Oh dear, he's disturbed the dwarves.
C
19000	CALL RSPEAK(136)
C
C  Exit code.  will eventually include scoring.  for now, however, ...
C
C  The present scoring algorithm is as follows:
C     objective:          points:        present total possible:
C  getting well into cave   25                    25
C  each treasure < chest    12                    60
C  treasure chest itself    14                    14
C  each treasure > chest    16                   144
C  surviving             (MAX-NUM)*10             30
C  not quitting              4                     4
C  reaching "closng"        25                    25
C  "closed": quit/killed    10
C            klutzed        25
C            wrong way      30
C            success        45                    45
C  came to Witt's End        1                     1
C  Round out the total       2                     2
C                                       total:   350
C  (Points can also be deducted for using hints.)
C
20000	SCORE=0
	MXSCOR=0
C
C  First tally up the treasures.  Must be in building and not broken.
C  Give the poor guy 2 points just for finding each treasure.
C
	DO 20010 I=50,MAXTRS
	IF(PTEXT(I).EQ.0)GOTO 20010
	K=12
	IF(I.EQ.CHEST)K=14
	IF(I.GT.CHEST)K=16
	IF(PROP(I).GE.0)SCORE=SCORE+2
	IF(PLACE(I).EQ.3.AND.PROP(I).EQ.0)SCORE=SCORE+K-2
	MXSCOR=MXSCOR+K
20010	CONTINUE
C
C  Now look at how he finished and how far he got.  MAXDIE and NUMDIE tell us
C  how well he survived.  GAVEUP says whether he exited via QUIT.  DFLAG will
C  tell us if he ever got suitably deep into the cave.  CLOSNG still indicates
C  whether he reached the endgame.  And if he got as far as "cave closed"
C  (indicated by "CLOSED"), then bonus is zero for mundane exits or 133, 134,
C  135 if he blew it (so to speak).
C
	SCORE=SCORE+(MAXDIE-NUMDIE)*10
	MXSCOR=MXSCOR+MAXDIE*10
	IF(.NOT.(SCORNG.OR.GAVEUP))SCORE=SCORE+4
	MXSCOR=MXSCOR+4
	IF(DFLAG.NE.0)SCORE=SCORE+25
	MXSCOR=MXSCOR+25
	IF(CLOSNG)SCORE=SCORE+25
	MXSCOR=MXSCOR+25
	IF(.NOT.CLOSED)GOTO 20020
	IF(BONUS.EQ.0)SCORE=SCORE+10
	IF(BONUS.EQ.135)SCORE=SCORE+25
	IF(BONUS.EQ.134)SCORE=SCORE+30
	IF(BONUS.EQ.133)SCORE=SCORE+45
20020	MXSCOR=MXSCOR+45
C
C  Did he come to Witt's End as he should?
C
	IF(PLACE(MAGZIN).EQ.108)SCORE=SCORE+1
	MXSCOR=MXSCOR+1
C
C  Round it off.
C
	SCORE=SCORE+2
	MXSCOR=MXSCOR+2
C
C  Deduct points for hints.  HINTS < 4 are special; see database description.
C
	DO 20030 I=1,HNTMAX
20030	IF(HINTED(I))SCORE=SCORE-HINTS(I,2)
C
C  Return to SCORE command if that's where we came from.
C
	IF(SCORNG)GOTO 8241
C
C  That should be good enough.  Let's tell him all about it.
C
	PRINT 20100,SCORE,MXSCOR,TURNS
20100	FORMAT(/' You scored',I4,' out of a possible',I4,
     1  ', using',I5,' turns.')
C
	DO 20200 I=1,CLSSES
	IF(CVAL(I).GE.SCORE)GOTO 20210
20200	CONTINUE
	PRINT 20202
20202	FORMAT(' You just went off my scale!!'/)
	GOTO 25000
C
20210	CALL SPEAK(CTEXT(I))
	IF(I.EQ.CLSSES-1)GOTO 20220
	K=CVAL(I)+1-SCORE
	PRINT 20212,K
20212	FORMAT(' To achieve the next higher rating, you need',I3,
     1  ' more point',$)
	IF(K.EQ.1) PRINT 20213
	IF(K.NE.1) PRINT 20214
20213	FORMAT('.'/)
20214	FORMAT('s.'/)
	GOTO 25000
C
20220	PRINT 20222
20222	FORMAT(' To achieve the next higher rating ',
     1  'would be a neat trick!'/' Congratulations!!'/)
C
25000	RETURN
C
	END
