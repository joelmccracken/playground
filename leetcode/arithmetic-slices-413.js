/**
 * @param {number[]} A
 * @return {number}
 */
var numberOfArithmeticSlices = function(A) {
    var differences = new Array(A.length-1);

    // first make array of differences;
    // differences[n] =  A[n] - A[n+1]
    for(var i = 0; i < A.length - 1; i++) {
        differences[i] =  A[i] - A[i+1];
    }

    console.log(differences, A);
    var differenceCounts = {};

    runsOfDifferences = [];

    // make counts of equal thingers



    for(var i = 0; i < differences.length; i++) {
        var diff = differences[i];
        if(!differenceCounts[diff]) {
            differenceCounts[diff] = 1;
        } else {
            differenceCounts[diff] += 1;
        }
    }

    var totalDifferences = 0;

    for(var diff in differenceCounts) {
        var count = differenceCounts[diff];

        totalDifferences += nChoose2(count);
    }

    return totalDifferences;
};

function nChoose2(n) {
    // n choose k  = n!/(k!(n - k)!)
    // n choose 2  = n!/(2(n - 2)!)
    if(n <= 1)
        return 0;

    var top = factorial(n);
    var bottom = (2 * factorial(n - 2));

    var result = top / bottom;


    console.log({n: n, ret: result, top: top, bottom: bottom});

    return result;
}

function factorial(n) {
    if(n == 0)
        return 1;
    var total = n;

    for(var m = (n-1); m > 1; m--) {
        total *= m;
    }
    return total;
}

console.log(numberOfArithmeticSlices(

    [1,2,3,8,9,10]

));
