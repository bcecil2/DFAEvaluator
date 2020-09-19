type Node = (String,Bool)

type Edge = (Node,Node,[Char])

type Graph = [Edge]

start = ("q0",False)
rem2 = ("q1",False)
rem1 = ("q2",False)
rem0 = ("q3",True)

s0 = (start,rem0,['0','3','6','9'])
s2 = (start,rem2,['8','2','5'])
s1 = (start,rem1,['1','7','4'])

loop1 = (rem1,rem1,['0','3','6','9'])
loop2 = (rem2,rem2,['0','3','6','9'])
loop0 = (rem0,rem0,['0','3','6','9'])

twoto0 = (rem2,rem0,['1','4','7']) 
zeroto2 = (rem0,rem2,['5','2','8'])

twoto1 = (rem2,rem1,['5','2','8'])
oneto2 = (rem1,rem2,['1','7','4'])

oneto0 = (rem1,rem0,['8','2','5'])
zeroto1 = (rem0,rem1,['1','4','7'])


-- A DFA that recognizes numbers that are divisible by three written in base 10
mod3 = [s0,s2,s1,loop0,loop1,loop2,twoto0,zeroto2,twoto1,oneto2,oneto0,zeroto1]


collectAll :: Graph -> Node -> Char -> Edge
collectAll g (n,_) c = head $ filter (\((n1,_), (n2,_) ,acceptors) -> (n1 == n) && (c `elem` acceptors)) g


validate :: Graph -> Node -> String -> Bool
validate _ (_,True) "" = True
validate _ (_,False) "" = False
validate g n (x:xs) = validate g nextNode xs 
                    where (_,nextNode,_) = collectAll g n x

main = print $ validate mod3 start "5999971004" 