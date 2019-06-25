// This method returns the nth Fibonacci number
function int fibonacci(int input){
    var int i1;
    var int i2;
    
    i1 = 1;
    i2 = 1;

    while(input > 2){
      var int temp;
      temp = i2;
      i2 = i1 + i2;
      i1 = i2;
      input = input - 1;
    }
    return i2;
}

function int main(*int input) {
  var int tenth;
  tenth = fibonacci(10);
  return 0;

}