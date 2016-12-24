/**
 * @param {character[][]} board
 * @return {number}
 */
var countBattleships = function(board) {
    console.log(board);
    var count = 0;
    var foundBattleship = false;
    for(var row = 0; row < board.length; row++) {
        for(var col = 0; col < board[row].length; col++) {
            if( board[row][col] == 'X' &&
                (col == 0 || (col > 0 && board[row][col-1] != 'X')) &&
                (row == 0 || (row > 0 && board[row-1][col] != 'X'))) {

                count++;
            }
        }
    }
    return count;
};

input1 = "\
X..X\n\
...X\n\
...X\n\
...X";


function toArray(str) {
    return str
        .split("\n")
        .filter(function(x) {
            return x != '';
        }).map(function(x){
            return x.split('');
        });
}


console.log( countBattleships(toArray(input1)) );
