       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRUD-FUNCIONARIOS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-FUNC ASSIGN TO "FUNCIONARIOS.DAT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS FUNC-CODIGO
           FILE STATUS IS WS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-FUNC.
       01  REG-FUNC.
           05 FUNC-CODIGO        PIC 9(05).
           05 FUNC-NOME          PIC X(40).
           05 FUNC-CARGO         PIC X(25).
           05 FUNC-DT-ADM        PIC 9(08).
           05 FUNC-SALARIO       PIC 9(08)V99.

       WORKING-STORAGE SECTION.
       77  WS-STATUS             PIC XX.
       77  WS-OPCAO              PIC X.
       77  WS-ENTER              PIC X.
       77  WS-PROX-CODIGO        PIC 9(05) VALUE 1.
       77  WS-FIM-ARQUIVO        PIC X.
       77  WS-TOTAL-REGISTROS    PIC 9(05).
       77  WS-VALOR-EDITADO      PIC ZZZ.ZZ9,99.
       77  WS-INPUT-SALARIO      PIC X(15).
       77  WS-CONFIRMA           PIC X.
       77  WS-LINHA              PIC 99.
       77  WS-CODIGO-BUSCA       PIC 9(05).
       
       77  WS-TEMP-NOME          PIC X(40).
       77  WS-TEMP-CARGO         PIC X(25).
       77  WS-TEMP-DATA          PIC X(08).
       77  WS-TEMP-DATA-MASK     PIC X(10).
       77  WS-DATA-VALIDA        PIC X.
       
       01  WS-DATA-MASK.
           05 WS-DIA             PIC 99.
           05 FILLER             PIC X VALUE "/".
           05 WS-MES             PIC 99.
           05 FILLER             PIC X VALUE "/".
           05 WS-ANO             PIC 9999.
           
       01  WS-DATA-EDITADA.
           05 WS-DIA-ED          PIC 99.
           05 FILLER             PIC X VALUE "/".
           05 WS-MES-ED          PIC 99.
           05 FILLER             PIC X VALUE "/".
           05 WS-ANO-ED          PIC 9999.

       SCREEN SECTION.
       01  TELA-MENU.
           05 BLANK SCREEN.
           05 LINE 01 COL 10 VALUE "=======================================".
           05 LINE 02 COL 10 VALUE "       SISTEMA DE GESTAO DE RH         ".
           05 LINE 03 COL 10 VALUE "=======================================".
           05 LINE 05 COL 10 VALUE "1 - Incluir Funcionario".
           05 LINE 06 COL 10 VALUE "2 - Alterar Funcionario".
           05 LINE 07 COL 10 VALUE "3 - Excluir Funcionario".
           05 LINE 08 COL 10 VALUE "4 - Listar Todos".
           05 LINE 09 COL 10 VALUE "5 - Sair".
           05 LINE 11 COL 10 VALUE "Escolha uma opcao: ".
           05 COL PLUS 1 PIC X TO WS-OPCAO.

       01  TELA-LIMPA.
           05 BLANK SCREEN.
           05 LINE 01 COL 10 VALUE "=======================================".
           05 LINE 02 COL 10 VALUE "       SISTEMA DE GESTAO DE RH         ".
           05 LINE 03 COL 10 VALUE "=======================================".

       01  TELA-INCLUSAO.
           05 BLANK SCREEN.
           05 LINE 01 COL 10 VALUE "=======================================".
           05 LINE 02 COL 10 VALUE "       INCLUSAO DE FUNCIONARIO         ".
           05 LINE 03 COL 10 VALUE "=======================================".
           05 LINE 05 COL 10 VALUE "Codigo: ".
           05 LINE 05 COL 18 PIC 9(05) FROM WS-PROX-CODIGO.
           05 LINE 07 COL 10 VALUE "Nome: ".
           05 LINE 07 COL 20 PIC X(40) TO FUNC-NOME.
           05 LINE 08 COL 10 VALUE "Cargo: ".
           05 LINE 08 COL 20 PIC X(25) TO FUNC-CARGO.
           05 LINE 09 COL 10 VALUE "Data (DD/MM/AAAA): ".
           05 LINE 09 COL 29 PIC X(10) TO WS-TEMP-DATA-MASK.
           05 LINE 10 COL 10 VALUE "Salario: R$ ".
           05 LINE 10 COL 22 PIC X(15) TO WS-INPUT-SALARIO.
           05 LINE 12 COL 10 VALUE "Confirmar (S/N)? ".
           05 LINE 12 COL 28 PIC X TO WS-CONFIRMA.

       01  TELA-LISTAGEM-CABECALHO.
           05 BLANK SCREEN.
           05 LINE 01 COL 10 VALUE "=======================================".
           05 LINE 02 COL 10 VALUE "     LISTAGEM DE FUNCIONARIOS          ".
           05 LINE 03 COL 10 VALUE "=======================================".
           05 LINE 05 COL 01 VALUE "ID    NOME                CARGO           DATA          SALARIO".
           05 LINE 06 COL 01 VALUE "---------------------------------------------------------------".

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM ABRIR-ARQUIVO
           PERFORM UNTIL WS-OPCAO = "5"
               DISPLAY TELA-MENU
               ACCEPT TELA-MENU
               EVALUATE WS-OPCAO
                   WHEN "1" PERFORM INCLUIR-FUNC
                   WHEN "2" PERFORM ALTERAR-FUNC
                   WHEN "3" PERFORM EXCLUIR-FUNC
                   WHEN "4" PERFORM LISTAR-FUNC
               END-EVALUATE
           END-PERFORM
           CLOSE ARQ-FUNC
           STOP RUN.

       ABRIR-ARQUIVO.
           OPEN I-O ARQ-FUNC
           IF WS-STATUS = "35"
               OPEN OUTPUT ARQ-FUNC
               CLOSE ARQ-FUNC
               OPEN I-O ARQ-FUNC
           END-IF
           IF WS-STATUS NOT = "00" AND WS-STATUS NOT = "05"
               DISPLAY "Erro ao abrir arquivo: " WS-STATUS 
                       AT LINE 24 COL 10
               ACCEPT WS-ENTER AT LINE 24 COL 40
               STOP RUN
           END-IF
           PERFORM CALCULA-PROX-CODIGO.
           
       VALIDAR-DATA.
           MOVE "S" TO WS-DATA-VALIDA
           IF WS-TEMP-DATA-MASK(1:2) NOT NUMERIC OR
              WS-TEMP-DATA-MASK(4:2) NOT NUMERIC OR
              WS-TEMP-DATA-MASK(7:4) NOT NUMERIC
               MOVE "N" TO WS-DATA-VALIDA
           ELSE
               MOVE WS-TEMP-DATA-MASK(1:2) TO WS-TEMP-DATA(1:2)
               MOVE WS-TEMP-DATA-MASK(4:2) TO WS-TEMP-DATA(3:2)
               MOVE WS-TEMP-DATA-MASK(7:4) TO WS-TEMP-DATA(5:4)
           END-IF.

       INCLUIR-FUNC.
           MOVE "N" TO WS-DATA-VALIDA
           PERFORM UNTIL WS-DATA-VALIDA = "S"
               DISPLAY TELA-INCLUSAO
               ACCEPT TELA-INCLUSAO
               
               PERFORM VALIDAR-DATA
               IF WS-DATA-VALIDA = "N"
                   DISPLAY "Formato invalido! Use DD/MM/AAAA" 
                           AT LINE 11 COL 10
                   DISPLAY "Pressione Enter..." AT LINE 12 COL 10
                   ACCEPT WS-ENTER AT LINE 12 COL 30
               END-IF
           END-PERFORM
           
           IF WS-CONFIRMA = "S" OR WS-CONFIRMA = "s"
               MOVE WS-PROX-CODIGO TO FUNC-CODIGO
               MOVE WS-TEMP-DATA TO FUNC-DT-ADM
               IF WS-INPUT-SALARIO NOT = SPACES
                   MOVE FUNCTION NUMVAL(WS-INPUT-SALARIO) 
                       TO FUNC-SALARIO
               ELSE
                   MOVE ZERO TO FUNC-SALARIO
               END-IF
               
               WRITE REG-FUNC
               IF WS-STATUS = "00"
                   PERFORM CALCULA-PROX-CODIGO
                   DISPLAY "Registro incluido com sucesso!" 
                           AT LINE 14 COL 10
               ELSE
                   DISPLAY "Erro ao gravar: " WS-STATUS 
                           AT LINE 14 COL 10
               END-IF
           ELSE
               DISPLAY "Inclusao cancelada" AT LINE 14 COL 10
           END-IF
           
           DISPLAY "Pressione Enter..." AT LINE 16 COL 10
           ACCEPT WS-ENTER AT LINE 16 COL 30.

       LISTAR-FUNC.
           DISPLAY TELA-LISTAGEM-CABECALHO
           
           MOVE 0 TO WS-TOTAL-REGISTROS
           MOVE 0 TO FUNC-CODIGO
           MOVE "N" TO WS-FIM-ARQUIVO
           MOVE 7 TO WS-LINHA
           
           START ARQ-FUNC KEY NOT < FUNC-CODIGO
           IF WS-STATUS NOT = "00"
               DISPLAY "ARQUIVO VAZIO" AT LINE 07 COL 10
           ELSE
               PERFORM UNTIL WS-FIM-ARQUIVO = "S"
                   READ ARQ-FUNC NEXT 
                       AT END MOVE "S" TO WS-FIM-ARQUIVO
                       NOT AT END
                           ADD 1 TO WS-TOTAL-REGISTROS
                           MOVE FUNC-DT-ADM(1:2) TO WS-DIA
                           MOVE FUNC-DT-ADM(3:2) TO WS-MES
                           MOVE FUNC-DT-ADM(5:4) TO WS-ANO
                           MOVE FUNC-SALARIO TO WS-VALOR-EDITADO
                           
                           DISPLAY FUNC-CODIGO      AT LINE WS-LINHA COL 01
                           DISPLAY FUNC-NOME(1:18)  AT LINE WS-LINHA COL 07
                           DISPLAY FUNC-CARGO(1:15) AT LINE WS-LINHA COL 27
                           DISPLAY WS-DIA          AT LINE WS-LINHA COL 43
                           DISPLAY "/"             AT LINE WS-LINHA COL 45
                           DISPLAY WS-MES          AT LINE WS-LINHA COL 46
                           DISPLAY "/"             AT LINE WS-LINHA COL 48
                           DISPLAY WS-ANO          AT LINE WS-LINHA COL 49
                           DISPLAY WS-VALOR-EDITADO AT LINE WS-LINHA COL 54
                           
                           ADD 1 TO WS-LINHA
                           IF WS-LINHA > 20
                               DISPLAY "Enter para continuar..." 
                                       AT LINE 22 COL 10
                               ACCEPT WS-ENTER AT LINE 22 COL 35
                               DISPLAY TELA-LISTAGEM-CABECALHO
                               MOVE 7 TO WS-LINHA
                           END-IF
                   END-READ
               END-PERFORM
           END-IF
           
           DISPLAY "---------------------------------------------------------------" 
                   AT LINE WS-LINHA COL 01
           ADD 1 TO WS-LINHA
           DISPLAY "Total: " AT LINE WS-LINHA COL 01
           DISPLAY WS-TOTAL-REGISTROS AT LINE WS-LINHA COL 08
           DISPLAY " registro(s)" AT LINE WS-LINHA COL 14
           DISPLAY "Pressione Enter..." AT LINE WS-LINHA COL 30
           ACCEPT WS-ENTER AT LINE WS-LINHA COL 48.

       ALTERAR-FUNC.
           DISPLAY TELA-LIMPA
           DISPLAY "ID para alterar: " AT LINE 05 COL 10
           ACCEPT WS-CODIGO-BUSCA AT LINE 05 COL 27
           
           MOVE WS-CODIGO-BUSCA TO FUNC-CODIGO
           READ ARQ-FUNC
           
           IF WS-STATUS = "00"
               *> Formatar data para exibicao
               MOVE FUNC-DT-ADM(1:2) TO WS-DIA-ED
               MOVE FUNC-DT-ADM(3:2) TO WS-MES-ED
               MOVE FUNC-DT-ADM(5:4) TO WS-ANO-ED
               
               DISPLAY "Nome: "        AT LINE 07 COL 10
               DISPLAY FUNC-NOME       AT LINE 07 COL 20
               ACCEPT WS-TEMP-NOME     AT LINE 08 COL 20
               IF WS-TEMP-NOME NOT = SPACES 
                   MOVE WS-TEMP-NOME TO FUNC-NOME 
               END-IF
               
               DISPLAY "Cargo: "       AT LINE 09 COL 10
               DISPLAY FUNC-CARGO      AT LINE 09 COL 20
               ACCEPT WS-TEMP-CARGO    AT LINE 10 COL 20
               IF WS-TEMP-CARGO NOT = SPACES 
                   MOVE WS-TEMP-CARGO TO FUNC-CARGO 
               END-IF
               
               DISPLAY "Data: "        AT LINE 11 COL 10
               DISPLAY WS-DIA-ED       AT LINE 11 COL 20
               DISPLAY "/"             AT LINE 11 COL 22
               DISPLAY WS-MES-ED       AT LINE 11 COL 23
               DISPLAY "/"             AT LINE 11 COL 25
               DISPLAY WS-ANO-ED       AT LINE 11 COL 26
               DISPLAY "Nova Data (DD/MM/AAAA): " AT LINE 12 COL 10
               ACCEPT WS-TEMP-DATA-MASK AT LINE 12 COL 35
               
               IF WS-TEMP-DATA-MASK NOT = SPACES 
                   MOVE "N" TO WS-DATA-VALIDA
                   PERFORM VALIDAR-DATA
                   IF WS-DATA-VALIDA = "S"
                       MOVE WS-TEMP-DATA TO FUNC-DT-ADM
                   ELSE
                       DISPLAY "Data invalida! Mantendo original." 
                               AT LINE 13 COL 10
                   END-IF
               END-IF
               
               DISPLAY "Salario: "     AT LINE 14 COL 10
               DISPLAY FUNC-SALARIO    AT LINE 14 COL 20
               DISPLAY "Novo Salario: " AT LINE 15 COL 10
               ACCEPT WS-INPUT-SALARIO AT LINE 15 COL 24
               
               IF WS-INPUT-SALARIO NOT = SPACES 
                   MOVE FUNCTION NUMVAL(WS-INPUT-SALARIO) 
                       TO FUNC-SALARIO
               END-IF
               
               DISPLAY "Confirmar alteracao (S/N)? " AT LINE 17 COL 10
               ACCEPT WS-CONFIRMA AT LINE 17 COL 38
               
               IF WS-CONFIRMA = "S" OR WS-CONFIRMA = "s"
                   REWRITE REG-FUNC
                   IF WS-STATUS = "00"
                       DISPLAY "Alterado com sucesso!" AT LINE 19 COL 10
                   ELSE
                       DISPLAY "Erro ao alterar: " WS-STATUS 
                               AT LINE 19 COL 10
                   END-IF
               ELSE
                   DISPLAY "Alteracao cancelada" AT LINE 19 COL 10
               END-IF
           ELSE
               DISPLAY "Registro nao encontrado" AT LINE 07 COL 10
           END-IF
           
           DISPLAY "Pressione Enter..." AT LINE 21 COL 10
           ACCEPT WS-ENTER AT LINE 21 COL 30.

       EXCLUIR-FUNC.
           DISPLAY TELA-LIMPA
           DISPLAY "ID para excluir: " AT LINE 05 COL 10
           ACCEPT WS-CODIGO-BUSCA AT LINE 05 COL 27
           
           MOVE WS-CODIGO-BUSCA TO FUNC-CODIGO
           READ ARQ-FUNC
           
           IF WS-STATUS = "00"
               DISPLAY "DADOS DO REGISTRO:" AT LINE 07 COL 10
               DISPLAY "NOME : "       AT LINE 08 COL 10
               DISPLAY FUNC-NOME       AT LINE 08 COL 18
               DISPLAY "CARGO: "       AT LINE 09 COL 10
               DISPLAY FUNC-CARGO      AT LINE 09 COL 18
               
               DISPLAY "Confirma exclusao (S/N)?" AT LINE 11 COL 10
               ACCEPT WS-CONFIRMA AT LINE 11 COL 36
               
               IF WS-CONFIRMA = "S" OR WS-CONFIRMA = "s"
                   DELETE ARQ-FUNC
                   IF WS-STATUS = "00"
                       DISPLAY "REGISTRO EXCLUIDO!" AT LINE 13 COL 10
                       PERFORM CALCULA-PROX-CODIGO
                   ELSE
                       DISPLAY "ERRO AO EXCLUIR: " WS-STATUS 
                               AT LINE 13 COL 10
                   END-IF
               ELSE
                   DISPLAY "EXCLUSAO CANCELADA" AT LINE 13 COL 10
               END-IF
           ELSE
               DISPLAY "REGISTRO NAO ENCONTRADO" AT LINE 07 COL 10
           END-IF
           
           DISPLAY "Pressione Enter..." AT LINE 15 COL 10
           ACCEPT WS-ENTER AT LINE 15 COL 30.

       CALCULA-PROX-CODIGO.
           MOVE 1 TO WS-PROX-CODIGO
           MOVE 0 TO FUNC-CODIGO
           START ARQ-FUNC KEY NOT < FUNC-CODIGO
           
           IF WS-STATUS = "00"
               MOVE "N" TO WS-FIM-ARQUIVO
               PERFORM UNTIL WS-FIM-ARQUIVO = "S"
                   READ ARQ-FUNC NEXT 
                       AT END MOVE "S" TO WS-FIM-ARQUIVO
                       NOT AT END
                           IF FUNC-CODIGO >= WS-PROX-CODIGO
                               COMPUTE WS-PROX-CODIGO = 
                                       FUNC-CODIGO + 1
                           END-IF
                   END-READ
               END-PERFORM
           ELSE
               MOVE 1 TO WS-PROX-CODIGO
           END-IF.
		   