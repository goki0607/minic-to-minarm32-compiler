// This is a very bad implementation of pow
function int pow(int base, int exponent){
    if (exponent == 0){
        return 1;
    }
    var int res;
    res = base;
    while(exponent > 1){
      res = res * base;
      exponent = exponent - 1;
    }
    return res;
}

function int main(*int input) {
  var int res;
  res = pow(10, 2);
  return 0;
}