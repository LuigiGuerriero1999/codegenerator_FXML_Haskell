#!/bin/bash

cd codegenerator_FXML_Haskell
cd out 
cd production
cd codegenerator_FXML_Haskell

java com.example.codegenerator.HelloApplication $1 $2 &

cd ..
cd ..
cd ..
cd generated

### Set initial time of file
LTIME=`stat -c %Z gi-gtk-generated.hs`
while true    
do
   ATIME=`stat -c %Z gi-gtk-generated.hs`
   # Frame #1
   printf "\r< generating Haskell code ......" 
   sleep 0.5
   # Frame #2 
   printf "\r> generating Haskell code ......" 
   sleep 0.5 

   if [[ "$ATIME" != "$LTIME" ]]
   then
       if [[ "$2" != "-runandgenerate" ]] 
       then 
        break
       else      
        make
        ./gi-gtk-generated
        LTIME=$ATIME
        break
       fi
   fi
   sleep 0.5
done

