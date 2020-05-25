#lang streamIt

void Schedule {
   initSched {
   }
   steadySched {
       HelloWorld();
   }
}

void->int filter IntSource() {
   // stateful filter
    init {
        int x = 0;
    }
    work push 1{
        x = x+1;
        push(x);
    }
}

int->void filter IntPrinter() {
    work pop 1 {
        print pop();
    }
}

void->void pipeline HelloWorld() {
    add IntSource();
    add IntPrinter();
}
