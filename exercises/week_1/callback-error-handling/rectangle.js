//module to calculate rectangle perimeters and areas

module.exports = (x,y,callback) => {
  if (x <= 0 || y <= 0) {
    setTimeout(() =>
        callback(new Error("Rectangle dimensions must be greater than 0: l = "
            + x + " and b = " + y),
          null),
        2000); // delaying for illustrative purposes
  }
  else {
    setTimeout(() =>
        callback(null,
          {
            perimeter: () => (2*(x+y)),
            area: () => (x*y)
          }),
        2000); // delaying for illustrative purposes
  }
}




