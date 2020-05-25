#lang streamIt

void Schedule {
   initSched {
   }
   steadySched {
       MovingAverage();
   }
}

void->void pipeline MovingAverage() {
  add IntSource();
  add Averager(10, 0);
  add IntPrinter();
}
int->int filter Averager(int n, int sum) {
  work pop 1 push 1 peek n {
    for (int i = 0; i < n; i= i+1) {
      sum = sum + peek(i);
      }
    push(sum/n);
    pop();
  }
}

void->int filter IntSource() {  // stateful filter
    init { int x = 0;}
    work push 1 {
	push(x);
	x = x+1;
    }
}

int->void filter IntPrinter() {
    work pop 1 { print pop(); }
}
