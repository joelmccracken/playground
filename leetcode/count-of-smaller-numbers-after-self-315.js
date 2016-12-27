/**
 * @param {number[]} nums
 * @return {number[]}
 */
var countSmaller = function(nums) {
    if(nums.length == 0) {
        return [];
    }

    var solution = new Array(nums.length);

    for(var i = nums.length - 1; i >= 0; i--) {
        var thisIncrement = 0;
        var thisSolution = 0;
        for(var j = (i+1); j < nums.length; j++) {
            if(nums[j] > nums[i]) {
                continue;
            } else if(nums[j] == nums[i]) {
                thisSolution = solution[j];
                break;
            // } else if(nums[j] == (nums[i] - 1)) {
            //     thisSolution = solution[j] + 1 ;
            //     break;
            // }
            } else {
                thisIncrement++;
            }
        }
//        console.log(i, nums[i], thisSolution+thisIncrement);
        solution[i] = (thisSolution + thisIncrement);
    }

    return solution;
};


//console.log(

    countSmaller(
    [26,78,27,100,33,67,90,23,66,5,38,7,35,23,52,22,83,51,98,69,81,32,78,28,94,13,2,97,3,76,99,51,9,21,84,66,65,36,100,41]
    )

//);
