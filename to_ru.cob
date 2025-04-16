IDENTIFICATION DIVISION.
PROGRAM-ID. CyrillicTextToMorse.

ENVIRONMENT DIVISION.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-TEXT PIC X(100).
01 WS-MORSE PIC X(500) VALUE SPACES.
01 WS-CHAR PIC X.
01 WS-CODE PIC X(5).
01 WS-INDEX PIC 9(3) VALUE 1.
01 WS-LENGTH PIC 9(3).

01 MORSE-TABLE.
   05 FILLER PIC X(2) VALUE "А ".
   05 FILLER PIC X(5) VALUE ".-    ".
   05 FILLER PIC X(2) VALUE "Б ".
   05 FILLER PIC X(5) VALUE "-...  ".
   05 FILLER PIC X(2) VALUE "В ".
   05 FILLER PIC X(5) VALUE ".--   ".
   05 FILLER PIC X(2) VALUE "Г ".
   05 FILLER PIC X(5) VALUE "--.   ".
   05 FILLER PIC X(2) VALUE "Д ".
   05 FILLER PIC X(5) VALUE "-..   ".
   05 FILLER PIC X(2) VALUE "Е ".
   05 FILLER PIC X(5) VALUE ".     ".
   05 FILLER PIC X(2) VALUE "Ё ".
   05 FILLER PIC X(5) VALUE ".     ".
   05 FILLER PIC X(2) VALUE "Ж ".
   05 FILLER PIC X(5) VALUE "...-- ".
   05 FILLER PIC X(2) VALUE "З ".
   05 FILLER PIC X(5) VALUE "--..  ".
   05 FILLER PIC X(2) VALUE "И ".
   05 FILLER PIC X(5) VALUE "..    ".
   05 FILLER PIC X(2) VALUE "Й ".
   05 FILLER PIC X(5) VALUE ".---  ".
   05 FILLER PIC X(2) VALUE "К ".
   05 FILLER PIC X(5) VALUE "-.-   ".
   05 FILLER PIC X(2) VALUE "Л ".
   05 FILLER PIC X(5) VALUE ".-..  ".
   05 FILLER PIC X(2) VALUE "М ".
   05 FILLER PIC X(5) VALUE "--    ".
   05 FILLER PIC X(2) VALUE "Н ".
   05 FILLER PIC X(5) VALUE "-.    ".
   05 FILLER PIC X(2) VALUE "О ".
   05 FILLER PIC X(5) VALUE "---   ".
   05 FILLER PIC X(2) VALUE "П ".
   05 FILLER PIC X(5) VALUE ".--.  ".
   05 FILLER PIC X(2) VALUE "Р ".
   05 FILLER PIC X(5) VALUE ".-.   ".
   05 FILLER PIC X(2) VALUE "С ".
   05 FILLER PIC X(5) VALUE "...   ".
   05 FILLER PIC X(2) VALUE "Т ".
   05 FILLER PIC X(5) VALUE "-     ".
   05 FILLER PIC X(2) VALUE "У ".
   05 FILLER PIC X(5) VALUE "..-   ".
   05 FILLER PIC X(2) VALUE "Ф ".
   05 FILLER PIC X(5) VALUE "..-.  ".
   05 FILLER PIC X(2) VALUE "Х ".
   05 FILLER PIC X(5) VALUE "....  ".
   05 FILLER PIC X(2) VALUE "Ц ".
   05 FILLER PIC X(5) VALUE "-.-.  ".
   05 FILLER PIC X(2) VALUE "Ч ".
   05 FILLER PIC X(5) VALUE "---.  ".
   05 FILLER PIC X(2) VALUE "Ш ".
   05 FILLER PIC X(5) VALUE "----  ".
   05 FILLER PIC X(2) VALUE "Щ ".
   05 FILLER PIC X(5) VALUE "--.-  ".
   05 FILLER PIC X(2) VALUE "Ъ ".
   05 FILLER PIC X(5) VALUE "--.-- ".
   05 FILLER PIC X(2) VALUE "Ы ".
   05 FILLER PIC X(5) VALUE "-.--. ".
   05 FILLER PIC X(2) VALUE "Ь ".
   05 FILLER PIC X(5) VALUE "-..-  ".
   05 FILLER PIC X(2) VALUE "Э ".
   05 FILLER PIC X(5) VALUE "..-.. ".
   05 FILLER PIC X(2) VALUE "Ю ".
   05 FILLER PIC X(5) VALUE "..--  ".
   05 FILLER PIC X(2) VALUE "Я ".
   05 FILLER PIC X(5) VALUE ".-.-  ".
   05 FILLER PIC X(2) VALUE "1 ".
   05 FILLER PIC X(5) VALUE ".---- ".
   05 FILLER PIC X(2) VALUE "2 ".
   05 FILLER PIC X(5) VALUE "..--- ".
   05 FILLER PIC X(2) VALUE "3 ".
   05 FILLER PIC X(5) VALUE "...-- ".
   05 FILLER PIC X(2) VALUE "4 ".
   05 FILLER PIC X(5) VALUE "....- ".
   05 FILLER PIC X(2) VALUE "5 ".
   05 FILLER PIC X(5) VALUE "..... ".
   05 FILLER PIC X(2) VALUE "6 ".
   05 FILLER PIC X(5) VALUE "-.... ".
   05 FILLER PIC X(2) VALUE "7 ".
   05 FILLER PIC X(5) VALUE "--... ".
   05 FILLER PIC X(2) VALUE "8 ".
   05 FILLER PIC X(5) VALUE "---.. ".
   05 FILLER PIC X(2) VALUE "9 ".
   05 FILLER PIC X(5) VALUE "----. ".
   05 FILLER PIC X(2) VALUE "0 ".
   05 FILLER PIC X(5) VALUE "----- ".
   05 FILLER PIC X(2) VALUE " ".
   05 FILLER PIC X(5) VALUE "| ".
   05 FILLER PIC X(2) VALUE ".".
   05 FILLER PIC X(5) VALUE ".-.-.- ".
   05 FILLER PIC X(2) VALUE ",".
   05 FILLER PIC X(5) VALUE "--..-- ".
   05 FILLER PIC X(2) VALUE "?".
   05 FILLER PIC X(5) VALUE "..--.. ".
   05 FILLER PIC X(2) VALUE "'".
   05 FILLER PIC X(5) VALUE ".----. ".
   05 FILLER PIC X(2) VALUE "!".
   05 FILLER PIC X(5) VALUE "-.-.-- ".
   05 FILLER PIC X(2) VALUE "/".
   05 FILLER PIC X(5) VALUE "-..-.".
   05 FILLER PIC X(2) VALUE "(".
   05 FILLER PIC X(5) VALUE "-.--.".
   05 FILLER PIC X(2) VALUE ")".
   05 FILLER PIC X(5) VALUE "-.--.-".
   05 FILLER PIC X(2) VALUE "&".
   05 FILLER PIC X(5) VALUE ".-...".
   05 FILLER PIC X(2) VALUE ":".
   05 FILLER PIC X(5) VALUE "---...".
   05 FILLER PIC X(2) VALUE ";".
   05 FILLER PIC X(5) VALUE "-.-.-.".
   05 FILLER PIC X(2) VALUE "=".
   05 FILLER PIC X(5) VALUE "-...-".

PROCEDURE DIVISION.
   DISPLAY "Enter text: " WITH NO ADVANCING.
   ACCEPT WS-TEXT.
   MOVE FUNCTION LENGTH(WS-TEXT) TO WS-LENGTH.

   PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > WS-LENGTH
       MOVE FUNCTION UPPER-CASE(WS-TEXT(WS-INDEX:1)) TO WS-CHAR
       PERFORM FIND-MORSE
       MOVE WS-CODE TO WS-MORSE(WS-INDEX:)
   END-PERFORM.

   DISPLAY "Morse Code: " WS-MORSE.
   STOP RUN.

FIND-MORSE.
   EVALUATE WS-CHAR
       WHEN "А" MOVE ".-    " TO WS-CODE
       WHEN "Б" MOVE "-...  " TO WS-CODE
       WHEN "В" MOVE ".--   " TO WS-CODE
       WHEN "Г" MOVE "--.   " TO WS-CODE
       WHEN "Д" MOVE "-..   " TO WS-CODE
       WHEN "Е" MOVE ".     " TO WS-CODE
       WHEN "Ё" MOVE ".     " TO WS-CODE
       WHEN "Ж" MOVE "...-- " TO WS-CODE
       WHEN "З" MOVE "--..  " TO WS-CODE
       WHEN "И" MOVE "..    " TO WS-CODE
       WHEN "Й" MOVE ".---  " TO WS-CODE
       WHEN "К" MOVE "-.-   " TO WS-CODE
       WHEN "Л" MOVE ".-..  " TO WS-CODE
       WHEN "М" MOVE "--    " TO WS-CODE
       WHEN "Н" MOVE "-.    " TO WS-CODE
       WHEN "О" MOVE "---   " TO WS-CODE
       WHEN "П" MOVE ".--.  " TO WS-CODE
       WHEN "Р" MOVE ".-.   " TO WS-CODE
       WHEN "С" MOVE "...   " TO WS-CODE
       WHEN "Т" MOVE "-     " TO WS-CODE
       WHEN "У" MOVE "..-   " TO WS-CODE
       WHEN "Ф" MOVE "..-.  " TO WS-CODE
       WHEN "Х" MOVE "....  " TO WS-CODE
       WHEN "Ц" MOVE "-.-.  " TO WS-CODE
       WHEN "Ч" MOVE "---.  " TO WS-CODE
       WHEN "Ш" MOVE "----  " TO WS-CODE
       WHEN "Щ" MOVE "--.-  " TO WS-CODE
       WHEN "Ъ" MOVE "--.-- " TO WS-CODE
       WHEN "Ы" MOVE "-.--. " TO WS-CODE
       WHEN "Ь" MOVE "-..-  " TO WS-CODE
       WHEN "Э" MOVE "..-.. " TO WS-CODE
       WHEN "Ю" MOVE "..--  " TO WS-CODE
       WHEN "Я" MOVE ".-.-  " TO WS-CODE
       WHEN "1" MOVE ".---- " TO WS-CODE
       WHEN "2" MOVE "..--- " TO WS-CODE
       WHEN "3" MOVE "...-- " TO WS-CODE
       WHEN "4" MOVE "....- " TO WS-CODE
       WHEN "5" MOVE "..... " TO WS-CODE
       WHEN "6" MOVE "-.... " TO WS-CODE
       WHEN "7" MOVE "--... " TO WS-CODE
       WHEN "8" MOVE "---.. " TO WS-CODE
       WHEN "9" MOVE "----. " TO WS-CODE
       WHEN "0" MOVE "----- " TO WS-CODE
       WHEN " " MOVE "| " TO WS-CODE
       WHEN "." MOVE ".-.-.- " TO WS-CODE
       WHEN "," MOVE "--..-- " TO WS-CODE
       WHEN "?" MOVE "..--.. " TO WS-CODE
       WHEN "'" MOVE ".----. " TO WS-CODE
       WHEN "!" MOVE "-.-.-- " TO WS-CODE
       WHEN "/" MOVE "-..-." TO WS-CODE
       WHEN "(" MOVE "-.--." TO WS-CODE
       WHEN ")" MOVE "-.--.-" TO WS-CODE
       WHEN "&" MOVE ".-..." TO WS-CODE
       WHEN ":" MOVE "---..." TO WS-CODE
       WHEN ";" MOVE "-.-.-." TO WS-CODE
       WHEN "=" MOVE "-...-" TO WS-CODE
       WHEN OTHER MOVE "     " TO WS-CODE
   END-EVALUATE.

   EXIT.
