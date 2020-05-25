#lang streamIt
void Schedule {
   initSched {
       LoopTestA(5);
       LoopTestB(5);
   }
   steadySched {
   }
}

int->int filter LoopTestA(int a) {
   for( int i=0; i < a; i = i+1) {
        print i;
   }
}

int->int filter LoopTestB(int b) {
   for( int i=0; i <= b; i = i+1) {
        print i;
   }
}