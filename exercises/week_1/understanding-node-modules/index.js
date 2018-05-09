//example to eval to area & perimeter of a rectangle

var rect = require('./rectangle');

function solveRect(l,b) {
  console.log("Solving for rectangle with l = " + l + " and b = " + b);

  if (l <= 0 || b <= 0) {
    console.log("rectangle dimenstions must be greater than 0: l = " + l + " and b = " + b);
  }
  else {
    console.log("The area of the rectangle is " + rect.area(l,b));
    console.log("The perimeter of the rectangle is " + rect.perimeter(l,b));
  }
}


solveRect(2,4);
solveRect(3,5);
solveRect(5,0);
solveRect(-3,5);

