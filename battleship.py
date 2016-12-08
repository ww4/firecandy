from random import randint

board = []
alpha = ('A','B','C','C','E')
num = ('1','2','3','4','5')

for x in range(5):
    board.append(["O"] * 5)

def print_board(board):
    for row in board:
        print (' '.join(row))

print ("Let's play Battleship!")
print ("Turn 1")
print_board(board)

def random_row(board):
    return randint(0, len(board) - 1)

def random_col(board):
    return randint(0, len(board[0]) - 1)

ship_row = random_row(board)
ship_col = random_col(board)
print ('ship position: ' + str (alpha[ship_row]) + str(ship_col + 1))

def take_a_turn():
    for turn in range(4):
        for guess in range(1000):
            guess_pos = str(input("Guess Position:"))
            if guess_pos == 'exit' or guess_pos == 'stop':
                return

            elif guess_pos != "" and len(guess_pos)<3:
                guess_row = (guess_pos[0])
                guess_row = guess_row.upper()
                guess_col = (guess_pos[1])
                if (guess_row in alpha) and (guess_col in num):
                    guess_row = int(alpha.index(guess_row))
                    guess_col = int(guess_col)-1
                    break
                else:
                    print ("Oops, that\'s not even in the ocean")
            elif guess_pos == "":
                continue
                    
            else:
                print ("Oops, invalid input.")

        if guess_row == ship_row and guess_col == ship_col:
            print ("Congratulations! You sunk my battleship!")
            break
        else:
            if(board[guess_row][guess_col] == "X"):
                print ("You guessed that one already.")
            else:
                print ("You missed my battleship!")
                board[int(guess_row)][int(guess_col)] = "X"
            # Print (turn + 1) here!
            if turn == 3:
                print ("Game Over")
            else:
                print ("Turn", turn + 2)
                print_board(board)
   
take_a_turn()