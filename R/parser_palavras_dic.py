from math import log
# Build a cost dictionary, assuming Zipf's law and cost = -math.log(probability).
words = open("R/words-by-frequency.txt").read().split()
# wordcost 
# dicionário com a lista de palavras em que a chave é a ordem de cada palavra
# ex: {0: palavra mais frequente, 1: 2a palavra mais frequente, ..., n: palavra menos frequente}
# o dicionário é uma tupla com a palavra e seu plog.
wordcost = dict((k, log((i+1)*log(len(words)))) for i,k in enumerate(words))

# palavra mais longa do corpus
maxword = max(len(x) for x in words)

def infer_spaces(s):
    """Uses dynamic programming to infer the location of spaces in a string
    without spaces."""

    # Find the best match for the i first characters, assuming cost has
    # been built for the i-1 first characters.
    # Returns a pair (match_cost, match_length).
    def best_match(i):
        candidates = enumerate(reversed(cost[max(0, i-maxword):i]))
        return min((c + wordcost.get(s[i-k-1:i], 9e999), k+1) for k,c in candidates)

    # Build the cost array.
    cost = [0]
    for i in range(1,len(s)+1):
        c,k = best_match(i)
        cost.append(c)
        # na primeira iteração:
        # i = 1, cost=[0]
        # best_match(1) -> 
        #     candidates = uma lista de tuplas contendo: 
        #     - índice do valor do vetor cost retornado
        #     - valor do vetor cost, que iniciará no máximo entre (0 e i-maxword) e terminará em i
        #     essa lista será invertida em candidates.
        #     na primeira iteração:
        #         candidates = [(0,0)]
        #     retorna o minimo em uma lista de tuplas que itera sobre candidates (min primeiro numero da tupla)
        #         (cost + custo da palavra na posição s[i-k-1] ou 9e999, k+1), k é o índice de candidates
        #           na primeira iteração retorna: (0 + custo da palavra em s[1-0-1=0] ou 9e99),1)
        #           na prática: (9e999, 1) ou custo de uma letra x (x,1).
        # cost terá agora o valor [0, x ou 9e999]. 

    # Backtrack to recover the minimal-cost string.
    out = []
    i = len(s)
    while i>0:
        c,k = best_match(i)
        print(c,k)
        assert c == cost[i]
        out.append(s[i-k:i])
        i -= k
        
    return " ".join(reversed(out))

infer_spaces("thefoxisnice")

