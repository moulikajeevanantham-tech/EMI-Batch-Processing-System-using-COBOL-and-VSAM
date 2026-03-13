 IDENTIFICATION DIVISION.                     
       PROGRAM-ID. EMIPROC.                         
       ENVIRONMENT DIVISION.                        
       INPUT-OUTPUT SECTION.                        
       FILE-CONTROL.                                
           SELECT EMI-FILE ASSIGN TO EMIIN          
           ORGANIZATION IS SEQUENTIAL               
           FILE STATUS IS WS-EMI-STATUS.            
           SELECT ACCOUNT-FILE ASSIGN TO KSDSACC    
           ORGANIZATION IS INDEXED                  
           ACCESS MODE IS DYNAMIC                   
           RECORD KEY IS ACC-NO                     
           FILE STATUS IS WS-ACC-STATUS.            
       DATA DIVISION.                               
       FILE SECTION.                                
       FD EMI-FILE.                                                                              
       01 EMI-REC.                                  
           05 EMI-ACC-NO PIC X(10).                 
           05 EMI-AMOUNT PIC 9(7).                  
           05 EMI-FILLER PIC X(63).                 
       FD ACCOUNT-FILE.                                                                          
       01 ACCOUNT-REC.                              
           05 ACC-NO PIC X(10).                 
           05 ACC-NAME PIC X(30).               
           05 ACC-TYPE PIC X(1).                
           05 ACC-BAL PIC 9(7).                 
           05 ACC-STATUS PIC X(1).              
       WORKING-STORAGE SECTION.                 
       01 WS-EMI-STATUS PIC XX.                 
       01 WS-ACC-STATUS PIC XX.                 
       01 WS-EOF PIC X VALUE 'N'.               
       01 WS-PENALTY PIC 9(5) VALUE 250.        
       01 WS-TOTAL-RECORDS PIC 9(5) VALUE 0.    
       01 WS-SUCCESS PIC 9(5) VALUE 0.          
       01 WS-FAILED PIC 9(5) VALUE 0.           
       01 WS-TOTAL-EMI PIC 9(9) VALUE 0.        
       01 WS-TOTAL-PENALTY PIC 9(9) VALUE 0.    
       PROCEDURE DIVISION.                      
       MAIN-PARA.                               
           OPEN INPUT EMI-FILE                  
                I-O ACCOUNT-FILE                
           PERFORM UNTIL WS-EOF = 'Y'           
              READ EMI-FILE                     
                 AT END                         
                    MOVE 'Y' TO WS-EOF          
                 NOT AT END                     
                      ADD 1 TO WS-TOTAL-RECORDS            
                   PERFORM PROCESS-EMI                  
             END-READ                                   
          END-PERFORM                                   
          PERFORM DISPLAY-SUMMARY                       
          CLOSE EMI-FILE ACCOUNT-FILE                   
          STOP RUN.                                     
      PROCESS-EMI.                                      
          MOVE EMI-ACC-NO TO ACC-NO                     
          READ ACCOUNT-FILE                             
            INVALID KEY                                 
               ADD 1 TO WS-FAILED                       
            NOT INVALID KEY                             
               PERFORM VALIDATE-ACCOUNT                 
          END-READ.                                     
      VALIDATE-ACCOUNT.                                 
          IF ACC-STATUS NOT = 'A'                       
             ADD 1 TO WS-FAILED                         
          ELSE                                          
             IF ACC-BAL >= EMI-AMOUNT                   
                SUBTRACT EMI-AMOUNT FROM ACC-BAL        
                ADD EMI-AMOUNT TO WS-TOTAL-EMI          
                ADD 1 TO WS-SUCCESS                     
             ELSE                                       
                SUBTRACT WS-PENALTY FROM ACC-BAL        
                ADD WS-PENALTY TO WS-TOTAL-PENALTY      
                ADD 1 TO WS-FAILED                      
              END-IF                                                 
              REWRITE ACCOUNT-REC                                    
           END-IF.                                                   
       DISPLAY-SUMMARY.                                              
           DISPLAY "------------ EMI BATCH SUMMARY ------------"     
           DISPLAY "TOTAL EMI RECORDS : " WS-TOTAL-RECORDS           
           DISPLAY "SUCCESSFUL EMI : " WS-SUCCESS                    
           DISPLAY "FAILED EMI : " WS-FAILED                         
           DISPLAY "TOTAL EMI COLLECTED : " WS-TOTAL-EMI             
           DISPLAY "TOTAL PENALTIES : " WS-TOTAL-PENALTY             
           DISPLAY "BATCH COMPLETED SUCCESSFULLY"                    
           DISPLAY "-------------------------------------------".    

