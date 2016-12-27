/**
 * @param {number} num
 * @return {number[]}
 */
var countBits = function(num) {
    var output = [];
    for(var i = 0; i <= num; i++) {
        var i_count = 0;
        var x = i;
        while( x > 0 ) {
            if(x & 1) {
                i_count ++;
            }
            x = x >>> 1;
        }
        output.push(i_count);
    }
    return output;
};

var countBits = function(num) {
    var output = [];
    var offsetForIteratorFromBeginning = 0;
    var nextPointToRestart = 1;
    for(var i = 0; i <= num; i++) {
        if(i == nextPointToRestart) {
            output.push(1);
            offsetForIteratorFromBeginning = (nextPointToRestart );
            nextPointToRestart *= 2;
        } else if(i < nextPointToRestart) {
            var repeatedIndex = (i - offsetForIteratorFromBeginning);
            if(output[repeatedIndex] === undefined) {
                output.push(0);
            } else {
                var val = 1 + output[repeatedIndex];
                output.push(val);
            }
        } else {
            throw new Exception("bro how did I get here");
        }
    }
    return output;
}

var num = 10;
console.log(countBits(num));
console.log(countBits2(num));
