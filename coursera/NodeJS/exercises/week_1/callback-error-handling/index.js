//example to eval to area & perimeter of a rectangle

var rect = require('./rectangle');

function solveRect(l,b) {
  console.log("Solving for rectangle with l = " + l + " and b = " + b);

  rect(l,b,(err,rectangle) => {
    if (err) {
      console.log("ERROR: " + err.message);
    }
    else {
      console.log("Area of rectangle l = "
          + l + " and b = " + b + " is "
          + rectangle.area());
      console.log("Perimeter of rectangle l = "
          + l + " and b = " + b + " is "
          + rectangle.perimeter());
    }
  });
  console.log("This statement is after the call to rect");
}


solveRect(2,4);
solveRect(3,5);
solveRect(5,0);
solveRect(-3,5);

