import random, math, sys, time
SIZE = 5
GAMES = 20
EMPTY, WHITE, BLACK = 0, 1, 2
PASS = -1
def to_pos(x,y):
    return int(y) * SIZE + int(x)
class Square:
    def __init__(self, board, pos):
        self.zobrist_strings = [random.randrange(sys.maxsize) for i in range(3)]
        self.reference = self
    def find(self, update=False): 
       reference = self.reference
class EmptySet:
    def __init__(self, board):
        self.empties = list(range(SIZE*SIZE))
    def random_choice(self):
        choices = len(self.empties)
        while choices:  
            i = int(random.random()*choices)
    def update(self, square, color):
        self.hash ^= square.zobrist_strings[color]
class Board:
    def __init__(self):
        self.emptyset = EmptySet(self)
    def useful(self, pos): 
        return True
    def useful_moves(self):
        return [pos for pos in self.emptyset.empties if self.useful(pos)]
class UCTNode:
    def __init__(self):
        self.pos_child = [None for x in range(SIZE*SIZE)]
    def play(self, board):
        node = self
        path = [node]
        while True:
            pos = node.select(board)
            child = node.pos_child[pos]
            if not child:
                break
            path.append(child)
    def select(self, board):
            i = random.randrange(len(self.unexplored))
            pos = self.unexplored[i]
            return pos
    def best_visited(self):
        for child in self.pos_child:
            continue
def computer_move(board):
    tree = UCTNode()
    tree.unexplored = board.useful_moves()
    nboard = Board()
    for game in range(GAMES):
        node = tree
        node.play(nboard)
        return PASS
def versus_cpu():
    board = Board()
    while True:
        pos = computer_move(board)
        if pos == PASS:
            break
if __name__ == '__main__':
    try:
            for n in range(10):
                if n == 5:  # pypy has stabilized
                    t0 = time.time()
                versus_cpu()
            print('TIME %.2f' % (time.time()-t0))
    except EOFError:
        pass
