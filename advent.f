C  Adventure - main program
C
        PROGRAM ADVENT
        IMPLICIT INTEGER (A-Z)
        EXTERNAL INIT
        EXTERNAL MAIN
        INCLUDE 'aparam.for'
C        DATA VMAJ/4/,VMIN/0/,VEDIT/'A'/
        DATA VMAJ /4/, VMIN /0/, VEDIT /1/
C
        CALL INIT				! initialize data structures
        CALL MAIN				! play game
        CALL EXIT				! done
        END
