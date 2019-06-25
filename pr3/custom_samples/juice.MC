function int triple(int x){
  return x * 3;
}

function int juice(int x) {
  var int y;
  y = 0;
  while (x > 0) {
    y = y + triple(y);
    x = x - 1; 
  } 
  return y;
}
