      ******************************************************************
      ** 
      ** Source File Name = varinp.sqb 
      ** 
      ** Licensed Materials - Property of IBM 
      ** 
      ** (C) COPYRIGHT International Business Machines Corp. 1995, 2000
      ** All Rights Reserved. 
      ** 
      ** US Government Users Restricted Rights - Use, duplication or 
      ** disclosure restricted by GSA ADP Schedule Contract with IBM
      ** 
      ** PURPOSE: This sample program demonstrates the use of a CURSOR.
      **          The CURSOR is processed using dynamic SQL.This program
      **          obtains all managers in the STAFF tables of the SAMPLE
      **          database and changes their job from "Mgr" to "Clerk". 
      **          A ROLLBACK is done so that the SAMPLE database remains
      **          unchanged.                                            
      **          This program demonstrates the use of parameter markers
      **  
      **                                                                
      ** For more information about these samples see the README file. 
      ** 
      ** For more information on Programming in COBOL, see the: 
      **    -  "Programming in COBOL" section of the Application
      **       Development Guide. 
      ** 
      ** For more information on Building COBOL Applications, see the: 
      **    - "Building COBOL Applications" section of the Application
      **      Building Guide. 
      ** 
      ** For more information on the SQL language see the SQL Reference.
      ** 
      ******************************************************************

       Identification Division.
       Program-ID. "varinp".

       Data Division.
       Working-Storage Section.

           copy "sqlca.cbl".

           EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01 pname             pic x(10).
       01 dept              pic s9(4) comp-5.
       01 st                pic x(127).
       01 parm-var          pic x(5).
       01 userid            pic x(8).
       01 passwd.
         49 passwd-length   pic s9(4) comp-5 value 0.
         49 passwd-name     pic x(18).
           EXEC SQL END DECLARE SECTION END-EXEC.

       77 errloc          pic x(80).

       Procedure Division.
       Main Section.
           display "Sample COBOL program: VARINP".

      * Get database connection information. 
           display "Enter your user id (default none): " 
                with no advancing.
           accept userid.

           if userid = spaces
             EXEC SQL CONNECT TO sample END-EXEC
           else
             display "Enter your password : " with no advancing
             accept passwd-name.

      * Passwords in a CONNECT statement must be entered in a VARCHAR
      * format  with the length of the input string. 
           inspect passwd-name tallying passwd-length for characters
              before initial " ".

           EXEC SQL CONNECT TO sample USER :userid USING :passwd
               END-EXEC.
           move "CONNECT TO" to errloc.
           call "checkerr" using SQLCA errloc.

           move "SELECT name, dept FROM staff
      -         "   WHERE job = ? FOR UPDATE OF job" to st.
           EXEC SQL PREPARE s1 FROM :st END-EXEC.                       1
           move "PREPARE" to errloc.
           call "checkerr" using SQLCA errloc.

           EXEC SQL DECLARE c1 CURSOR FOR s1 END-EXEC.                  2

           move "Mgr" to parm-var.

           EXEC SQL OPEN c1 USING :parm-var END-EXEC                    3
           move "OPEN" to errloc.
           call "checkerr" using SQLCA errloc.

           move "Clerk" to parm-var.
           move "UPDATE staff SET job = ? WHERE CURRENT OF c1" to st.

           EXEC SQL PREPARE s2 from :st END-EXEC.                       4
           move "PREPARE S2" to errloc.
           call "checkerr" using SQLCA errloc.

      * call the FETCH and UPDATE loop. 
           perform Fetch-Loop thru End-Fetch-Loop
              until SQLCODE not equal 0.

           EXEC SQL CLOSE c1 END-EXEC.                                  7
           move "CLOSE" to errloc.
           call "checkerr" using SQLCA errloc.

           EXEC SQL ROLLBACK END-EXEC.
           move "ROLLBACK" to errloc.
           call "checkerr" using SQLCA errloc.
           DISPLAY "On second thought -- changes rolled back.".

           EXEC SQL CONNECT RESET END-EXEC.
           move "CONNECT RESET" to errloc.
           call "checkerr" using SQLCA errloc.
       End-Main.
           go to End-Prog.

       Fetch-Loop Section.
           EXEC SQL FETCH c1 INTO :pname, :dept END-EXEC.               5
           if SQLCODE not equal 0
              go to End-Fetch-Loop.
           display pname, " in dept. ", dept,
              " will be demoted to Clerk".

           EXEC SQL EXECUTE s2 USING :parm-var END-EXEC.                6
           move "EXECUTE" to errloc.
           call "checkerr" using SQLCA errloc.

       End-Fetch-Loop. exit.

       End-Prog.
           stop run.
