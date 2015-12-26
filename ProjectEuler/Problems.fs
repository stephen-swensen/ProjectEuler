module Problems

open Swensen
open System.Numerics
open Swensen.Combinatorics
//Stephen Swensen, Project Euler, May 4th 2010

//problem1
//Add all the natural numbers below one thousand that are multiples of 3 or 5.
let problem1a =
    {1..999}
    |> Seq.filter (fun i -> i % 3 = 0 || i % 5 = 0)
    |> Seq.sum

let problem1b =
    {1..999}
    |> Seq.fold (fun acc i -> if i % 3 = 0 || i % 5 = 0 then acc + i else acc) 0
    
let problem1c =    
    {1..999}
    |> Seq.sumBy (fun i -> if i % 3 = 0 || i % 5 = 0 then i else 0)
    
let problem1d = 
    let a = Seq.append {0..3..999} {0..5..999} |> Seq.sum
    let b = {0..15..999} |> Seq.sum
    a - b    
        
//problem2
//Find the sum of all the even-valued terms in the Fibonacci sequence which do not exceed four million.    
let problem2a =
    fib
    |> Seq.initInfinite
    |> Seq.takeWhile (fun i -> i <= 4000000) 
    |> Seq.filter (fun i -> i % 2 = 0)
    |> Seq.sum
    
//adapted from MihaiF's version in C#
let problem2b =
    let rec solver a b sum = 
        if b > 4000000 then sum 
        else solver b (a + b) (sum + (if b % 2 = 0 then b else 0))
    solver 1 1 0
    
//problem3 
////What is the largest prime factor of the number 600851475143 ?
let problem3a =
    let factorize n =
        let rec factorize n j list = 
            if n = 1I then list
            elif n % j = 0I then factorize (n/j) j (j::list)
            else factorize n (j + 1I) (list)
        factorize n 2I []
    600851475143I |> factorize |> List.head

//a less general implementation of 3a
let problem3b =
    let rec largestFactor n j largest = 
        if n = 1I then largest
        elif n % j = 0I then largestFactor (n/j) j j
        else largestFactor n (j + 1I) largest
    largestFactor 600851475143I 2I 1I
    
//problem4

//Find the largest palidromic number made from the product of two 3-digit numbers
let isPalindrome n =
    let rec isPalindrome (nstr:string) i j =
        if i > j then true 
        elif  nstr.[i] <> nstr.[j] then false
        else isPalindrome nstr (i+1) (j-1)
    let nstr = n |> string
    isPalindrome nstr 0 (nstr.Length - 1)

//fastest, reasonable use of iterative loop + mutable
let problem4a =        
    let mutable max = 0
    for i in 100..999 do
        for j in 100..999 do
            let p = i * j
            if p > max && p |> isPalindrome then max <- p    
    max

//notably slower since has to process isPalindrome for every possibility
let problem4b =
    seq { for i in 100..999 do for j in 100..999 do yield i * j }
    |> Seq.filter isPalindrome
    |> Seq.max

//still lower than 4a
let problem4c =
    seq { for i in 100..999 do for j in 100..999 do yield i * j }
    |> Seq.fold (fun acc i -> if i > acc && i |> isPalindrome then i else acc) 0


//problem5 
//What is the smallest number that is evenly divisible by all of the numbers from 1 to 20?
//only need to test 10..20, since 1..9 are all divisors of some number in 10..20
let problem5a =
    let rec find i = 
        if {11..20} |> Seq.forall (fun d -> i % d = 0) then i
        else find (i + 1)
    find (2*3*5*7*11*13*17*19) //has to at least have each prime in range a factor

//problem6
//Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.    
let problem6a =
    let range = {1..100}
    let sumOfSquares = range |> Seq.sumBy (fun x -> x * x)
    let sumSquared = pown (range |> Seq.sum) 2
    sumSquared - sumOfSquares

//problem7
//What is the 10001st prime number?
let problem7a =        
    let nthPrime n = 
        let rec nthPrime i p count =
            if count = n then p
            elif i |> isPrime then nthPrime (i+2) i (count+1)
            else nthPrime (i+2) p count
        nthPrime 1 1 0
        
    nthPrime 10001
        
//the \ char prevents newlines from being inserted
let digits1000 = "73167176531330624919225119674426574742355349194934\
96983520312774506326239578318016984801869478851843\
85861560789112949495459501737958331952853208805511\
12540698747158523863050715693290963295227443043557\
66896648950445244523161731856403098711121722383113\
62229893423380308135336276614282806444486645238749\
30358907296290491560440772390713810515859307960866\
70172427121883998797908792274921901699720888093776\
65727333001053367881220235421809751254540594752243\
52584907711670556013604839586446706324415722155397\
53697817977846174064955149290862569321978468622482\
83972241375657056057490261407972968652414535100474\
82166370484403199890008895243450658541227588666881\
16427171479924442928230863465674813919123162824586\
17866458359124566529476545682848912883142607690042\
24219022671055626321111109370544217506941658960408\
07198403850962455444362981230987879927244284909188\
84580156166097919133875499200524063689912560717606\
05886116467109405077541002256983155200055935729725\
71636269561882670428252483600823257530420752963450"

let problem8a =
    let mutable max = 0
    for i in 0..(digits1000.Length-5) do
        let mutable next = 1
        for j in 0..4 do
            next <- next * (digits1000.[i+j] |> string |> int)
        if next > max then max <- next
    max
       
let problem8b =
    {0..digits1000.Length-5}
    |> Seq.map
        (fun i -> 
             {0..4} 
             |> Seq.map (fun j -> digits1000.[i+j] |> string |> int) 
             |> Seq.fold (*) 1)
    |> Seq.max

//this one ain't so good.    
let problem8c =
    let rec find i max =
        if i > digits1000.Length-5 then max
        else
            let next = {0..4} 
                       |> Seq.map (fun j -> (digits1000.[i+j] |> string |> int)) 
                       |> Seq.fold (*) 1
            if next > max then find (i+1) next
            else find (i+1) max
    find 0 0
             
        
//problem 9
//Find the only Pythagorean triplet, {a, b, c}, for which a + b + c = 1000.
let problem9a =
    seq {for a in 3..332 do for b in 4..498 do yield (a, b, 1000 - a - b)}
    |> Seq.find (fun (a,b,c) -> (pown a 2) + (pown b 2) = (pown c 2))
    |> (fun (a,b,c) -> a*b*c)
    

//problem 10
//Calculate the sum of all the primes below two million.
let problem10a =
    let primes max = 
        let rec primes i plist =
            if i > max then plist
            elif i |> isPrime then primes (i+2) (i::plist)
            else primes (i+2) plist
        primes 3 [2] //start at 3, and with a list containing our only even prime
        
    primes 1999999 |> List.sumBy int64  //convert to int64 to avoid overflow

let sw = System.Diagnostics.Stopwatch()
sw.Start()        
let problem10b =
    (seq { for i in 3..2..1999999 do if i |> isPrime then yield i} //odd primes
    |> Seq.sumBy int64) + 2L //dont forget 2
sw.Stop()
printfn "%A" sw.ElapsedMilliseconds
sw.Reset()

sw.Start()    
//very nice! double in speed!
let problem10d = 
    let p = System.Linq.ParallelEnumerable.AsParallel(seq { for i in 3..2..1999999 -> i}) //odd primes
    let p2 = System.Linq.ParallelEnumerable.Where(p, fun i -> if i |> isPrime then true else false)
    System.Linq.ParallelEnumerable.Sum(p2,int64) + 2L
sw.Stop()
printfn "%A" sw.ElapsedMilliseconds    

sw.Reset()
sw.Start()    
open Microsoft.FSharp.Collections

//nice!  same stats as problem10d!
let problem10e = 
    ({3..2..1999999}
    |> PSeq.filter isPrime
    |> PSeq.sumBy int64) + 2L
sw.Stop()
printfn "%A" sw.ElapsedMilliseconds    

//this sucks
let problem10c =
    let asyncIsPrime i = async { if i |> isPrime then return i else return 0 } //odd primes
    let tasks = seq {for i in 3..2..1999999 -> asyncIsPrime i}
    ((Async.RunSynchronously (Async.Parallel tasks)) |> Seq.filter((<) 0) |> Seq.sumBy(int64)) + 2L

let problem10bb =
    (seq { for i in 1000000000..1000100000 do if i |> isPrime then yield i} //odd primes
    |> Seq.sumBy(fun i -> bigint(i))) + 2I //dont forget 2


//this is sometimes better, but sometimes worse

let problem10cc =
    let asyncIsPrime i = async { if i |> isPrime then return i else return 0 } //odd primes
    let tasks = seq {for i in 1000000000..1000100000 -> asyncIsPrime i}
    ((Async.RunSynchronously (Async.Parallel tasks)) |> Seq.filter((<) 0) |> Seq.sumBy(fun i -> bigint(i))) + 2I

//problem 11
//What is the greatest product of four numbers on the same straight line in the 20 by 20 grid?
let problem11a =
    let grid = array2D [[08;02;22;97;38;15;00;40;00;75;04;05;07;78;52;12;50;77;91;08];[49;49;99;40;17;81;18;57;60;87;17;40;98;43;69;48;04;56;62;00];[81;49;31;73;55;79;14;29;93;71;40;67;53;88;30;03;49;13;36;65];[52;70;95;23;04;60;11;42;69;24;68;56;01;32;56;71;37;02;36;91];[22;31;16;71;51;67;63;89;41;92;36;54;22;40;40;28;66;33;13;80];[24;47;32;60;99;03;45;02;44;75;33;53;78;36;84;20;35;17;12;50];[32;98;81;28;64;23;67;10;26;38;40;67;59;54;70;66;18;38;64;70];[67;26;20;68;02;62;12;20;95;63;94;39;63;08;40;91;66;49;94;21];[24;55;58;05;66;73;99;26;97;17;78;78;96;83;14;88;34;89;63;72];[21;36;23;09;75;00;76;44;20;45;35;14;00;61;33;97;34;31;33;95];[78;17;53;28;22;75;31;67;15;94;03;80;04;62;16;14;09;53;56;92];[16;39;05;42;96;35;31;47;55;58;88;24;00;17;54;24;36;29;85;57];[86;56;00;48;35;71;89;07;05;44;44;37;44;60;21;58;51;54;17;58];[19;80;81;68;05;94;47;69;28;73;92;13;86;52;17;77;04;89;55;40];[04;52;08;83;97;35;99;16;07;97;57;32;16;26;26;79;33;27;98;66];[88;36;68;87;57;62;20;72;03;46;33;67;46;55;12;32;63;93;53;69];[04;42;16;73;38;25;39;11;24;94;72;18;08;46;29;32;40;62;76;36];[20;69;36;41;72;30;23;88;34;62;99;69;82;67;59;85;74;04;36;16];[20;73;35;29;78;31;90;01;74;31;49;71;48;86;81;16;23;57;05;54];[01;70;54;71;83;51;54;69;16;92;33;48;61;43;52;01;89;19;67;48]]
    seq {for i in 0..16 do 
             for j in 0..16 do
                 yield grid.[i,j] * grid.[i+1,j] * grid.[i+2,j] * grid.[i+3,j] //horz
                 yield grid.[i,j] * grid.[i,j+1] * grid.[i,j+2] * grid.[i,j+3] //vert 
                 yield grid.[i,j] * grid.[i+1,j+1] * grid.[i+2,j+2] * grid.[i+3,j+3] //f-diag
                 if i-3 >= 0 then yield grid.[i,j] * grid.[i-1,j+1] * grid.[i-2,j+2] * grid.[i-3,j+3]} //b-diag 
    |> Seq.max
    
//simplified version of (b)
let problem11b =
    let grid = array2D [[08;02;22;97;38;15;00;40;00;75;04;05;07;78;52;12;50;77;91;08];[49;49;99;40;17;81;18;57;60;87;17;40;98;43;69;48;04;56;62;00];[81;49;31;73;55;79;14;29;93;71;40;67;53;88;30;03;49;13;36;65];[52;70;95;23;04;60;11;42;69;24;68;56;01;32;56;71;37;02;36;91];[22;31;16;71;51;67;63;89;41;92;36;54;22;40;40;28;66;33;13;80];[24;47;32;60;99;03;45;02;44;75;33;53;78;36;84;20;35;17;12;50];[32;98;81;28;64;23;67;10;26;38;40;67;59;54;70;66;18;38;64;70];[67;26;20;68;02;62;12;20;95;63;94;39;63;08;40;91;66;49;94;21];[24;55;58;05;66;73;99;26;97;17;78;78;96;83;14;88;34;89;63;72];[21;36;23;09;75;00;76;44;20;45;35;14;00;61;33;97;34;31;33;95];[78;17;53;28;22;75;31;67;15;94;03;80;04;62;16;14;09;53;56;92];[16;39;05;42;96;35;31;47;55;58;88;24;00;17;54;24;36;29;85;57];[86;56;00;48;35;71;89;07;05;44;44;37;44;60;21;58;51;54;17;58];[19;80;81;68;05;94;47;69;28;73;92;13;86;52;17;77;04;89;55;40];[04;52;08;83;97;35;99;16;07;97;57;32;16;26;26;79;33;27;98;66];[88;36;68;87;57;62;20;72;03;46;33;67;46;55;12;32;63;93;53;69];[04;42;16;73;38;25;39;11;24;94;72;18;08;46;29;32;40;62;76;36];[20;69;36;41;72;30;23;88;34;62;99;69;82;67;59;85;74;04;36;16];[20;73;35;29;78;31;90;01;74;31;49;71;48;86;81;16;23;57;05;54];[01;70;54;71;83;51;54;69;16;92;33;48;61;43;52;01;89;19;67;48]]

    let calc (i,j) (iu,ju) =
        {0..3} |> Seq.map (fun k -> grid.[i+(k*iu),j+(k*ju)]) |> Seq.fold (*) 1

    seq {for i in 0..16 do 
             for j in 0..16 do
                 yield calc (i,j) (1,0)
                 yield calc (i,j) (0,1)
                 yield calc (i,j) (1,1)
                 if i-3 >= 0 then yield calc (i,j) (-1,1) }
    |> Seq.max

//problem12
//What is the value of the first triangle number to have over five hundred divisors?
let problem12a = 
    let divisors n = seq {for i in 1..n/2 do if n % i = 0 then yield i}
    let rec find i last = 
        if last |> divisors |> Seq.length > 500 then last
        else find (i+1) (last+(i+1))
    find 1 1

let problem12b =
    let divisors n =
        let rec divisors n j list = 
            if n = 1I then list
            elif n % j = 0I then divisors (n/j) 2I (list @ j::(list |> List.map (fun x -> x*j)))
            else divisors n (j + 1I) (list)
        1I::(divisors n 2I []) |> List.toSeq |> Seq.distinct
        
    let rec find i last = 
        if last |> divisors |> Seq.length > 500 then last
        else find (i+1I) (last+(i+1I))
    find 1I 1I
    
let problem12c =
    let countDivisors n =
        let rec countDivisors flist count =
            match flist with 
            | [] -> count
            | f::list -> 
                let plist = list |> List.partition (fun i -> i = f)
                countDivisors (snd plist) (((fst plist |> List.length)+2)*count)
        countDivisors (factorize n) 1

    let rec find i last = 
        if last |> countDivisors > 500 then last
        else find (i+1) (last+(i+1))
    find 1 1
    
let problem12d =
    let countDivisors n =
        factorize n
        |> Seq.groupBy id
        |> Seq.fold (fun acc (f,s) -> ((Seq.length s) + 1)*acc) 1

    let rec find i last = 
        if last |> countDivisors > 500 then last
        else find (i+1) (last+(i+1))
    find 1 1

//problem 13
//Find the first ten digits of the sum of one-hundred 50-digit numbers.
let problem13a =    
    [37107287533902102798797998220837590246510135740250I;
    46376937677490009712648124896970078050417018260538I;
    74324986199524741059474233309513058123726617309629I;
    91942213363574161572522430563301811072406154908250I;
    23067588207539346171171980310421047513778063246676I;
    89261670696623633820136378418383684178734361726757I;
    28112879812849979408065481931592621691275889832738I;
    44274228917432520321923589422876796487670272189318I;
    47451445736001306439091167216856844588711603153276I;
    70386486105843025439939619828917593665686757934951I;
    62176457141856560629502157223196586755079324193331I;
    64906352462741904929101432445813822663347944758178I;
    92575867718337217661963751590579239728245598838407I;
    58203565325359399008402633568948830189458628227828I;
    80181199384826282014278194139940567587151170094390I;
    35398664372827112653829987240784473053190104293586I;
    86515506006295864861532075273371959191420517255829I;
    71693888707715466499115593487603532921714970056938I;
    54370070576826684624621495650076471787294438377604I;
    53282654108756828443191190634694037855217779295145I;
    36123272525000296071075082563815656710885258350721I;
    45876576172410976447339110607218265236877223636045I;
    17423706905851860660448207621209813287860733969412I;
    81142660418086830619328460811191061556940512689692I;
    51934325451728388641918047049293215058642563049483I;
    62467221648435076201727918039944693004732956340691I;
    15732444386908125794514089057706229429197107928209I;
    55037687525678773091862540744969844508330393682126I;
    18336384825330154686196124348767681297534375946515I;
    80386287592878490201521685554828717201219257766954I;
    78182833757993103614740356856449095527097864797581I;
    16726320100436897842553539920931837441497806860984I;
    48403098129077791799088218795327364475675590848030I;
    87086987551392711854517078544161852424320693150332I;
    59959406895756536782107074926966537676326235447210I;
    69793950679652694742597709739166693763042633987085I;
    41052684708299085211399427365734116182760315001271I;
    65378607361501080857009149939512557028198746004375I;
    35829035317434717326932123578154982629742552737307I;
    94953759765105305946966067683156574377167401875275I;
    88902802571733229619176668713819931811048770190271I;
    25267680276078003013678680992525463401061632866526I;
    36270218540497705585629946580636237993140746255962I;
    24074486908231174977792365466257246923322810917141I;
    91430288197103288597806669760892938638285025333403I;
    34413065578016127815921815005561868836468420090470I;
    23053081172816430487623791969842487255036638784583I;
    11487696932154902810424020138335124462181441773470I;
    63783299490636259666498587618221225225512486764533I;
    67720186971698544312419572409913959008952310058822I;
    95548255300263520781532296796249481641953868218774I;
    76085327132285723110424803456124867697064507995236I;
    37774242535411291684276865538926205024910326572967I;
    23701913275725675285653248258265463092207058596522I;
    29798860272258331913126375147341994889534765745501I;
    18495701454879288984856827726077713721403798879715I;
    38298203783031473527721580348144513491373226651381I;
    34829543829199918180278916522431027392251122869539I;
    40957953066405232632538044100059654939159879593635I;
    29746152185502371307642255121183693803580388584903I;
    41698116222072977186158236678424689157993532961922I;
    62467957194401269043877107275048102390895523597457I;
    23189706772547915061505504953922979530901129967519I;
    86188088225875314529584099251203829009407770775672I;
    11306739708304724483816533873502340845647058077308I;
    82959174767140363198008187129011875491310547126581I;
    97623331044818386269515456334926366572897563400500I;
    42846280183517070527831839425882145521227251250327I;
    55121603546981200581762165212827652751691296897789I;
    32238195734329339946437501907836945765883352399886I;
    75506164965184775180738168837861091527357929701337I;
    62177842752192623401942399639168044983993173312731I;
    32924185707147349566916674687634660915035914677504I;
    99518671430235219628894890102423325116913619626622I;
    73267460800591547471830798392868535206946944540724I;
    76841822524674417161514036427982273348055556214818I;
    97142617910342598647204516893989422179826088076852I;
    87783646182799346313767754307809363333018982642090I;
    10848802521674670883215120185883543223812876952786I;
    71329612474782464538636993009049310363619763878039I;
    62184073572399794223406235393808339651327408011116I;
    66627891981488087797941876876144230030984490851411I;
    60661826293682836764744779239180335110989069790714I;
    85786944089552990653640447425576083659976645795096I;
    66024396409905389607120198219976047599490197230297I;
    64913982680032973156037120041377903785566085089252I;
    16730939319872750275468906903707539413042652315011I;
    94809377245048795150954100921645863754710598436791I;
    78639167021187492431995700641917969777599028300699I;
    15368713711936614952811305876380278410754449733078I;
    40789923115535562561142322423255033685442488917353I;
    44889911501440648020369068063960672322193204149535I;
    41503128880339536053299340368006977710650566631954I;
    81234880673210146739058568557934581403627822703280I;
    82616570773948327592232845941706525094512325230608I;
    22918802058777319719839450180888072429661980811197I;
    77158542502016545090413245809786882778948721859617I;
    72107838435069186155435662884062257473692284509516I;
    20849603980134001723930671666823555245252804609722I;
    53503534226472524250874054075591789781264330331690I;]
    |> Seq.sum |> string |> Seq.take 10
    
//problem 14:

//The following iterative sequence is defined for the set of positive integers:
//
//n → n/2 (n is even)
//n → 3n + 1 (n is odd)
//
//Using the rule above and starting with 13, we generate the following sequence:
//13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
//
//It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
//
//Which starting number, under one million, produces the longest chain?
//
//NOTE: Once the chain starts the terms are allowed to go above one million.
let problem14a = 
    let collatz n =
        let rec collatz n count =
            let count = count + 1L
            if n = 1L then count
            elif n % 2L = 0L then collatz (n/2L) count
            else collatz ((3L*n)+1L) count
        collatz n 0L
        
    {1L..999999L} |> Seq.map (fun i -> (i, collatz i)) |> Seq.maxBy snd |> fst
    
let problem14b = 
    let mem = new System.Collections.Generic.Dictionary<int64,int64>()
    let collatz m =
        let rec collatz n count =
            let count = count + 1L
            if mem.ContainsKey n then
                let count = mem.[n] + count - 1L
                mem.Add(m, count)
                count
            elif n = 1L then 
                mem.Add(m, count)
                count
            elif n % 2L = 0L then collatz (n/2L) count
            else collatz ((3L*n)+1L) count
        collatz m 0L
        
    {1L..999999L} |> Seq.map (fun i -> (i, collatz i)) |> Seq.maxBy snd |> fst
    

//problem15
///Starting in the top left corner in a 20 by 20 grid, how many routes are there to the bottom right corner?
//by mathemtatical proof, using multiset permutation counting formula
let problem15a =
    let multiset m = (factorialI m) / ((factorialI (m/2I))*(factorialI (m/2I)))
    multiset 40I

//problem16    
//What is the sum of the digits of the number 2**1000?
let problem16a =
    bigint.Pow(2I, 1000) |> string |> Seq.sumBy (fun c -> c |> string |> int)
    
//problem 17: 
//If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used? 
let problem17c =
    "OneThousand".Length +
    
    "OneHundred".Length +
    "TwoHundred".Length +
    "ThreeHundred".Length +
    "FourHundred".Length +
    "FiveHundred".Length +
    "SixHundred".Length +
    "SevenHundred".Length +
    "EightHundred".Length +
    "NineHundred".Length +
    
    99*"OneHundredAnd".Length +
    "OneTwoThreeFourFiveSixSevenEightNineTenElevenTwelveThirteenFourteenFifteenSixteenSeventeenEighteenNineteen".Length +
    10*"Twenty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Thirty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Forty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Fifty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Sixty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Seventy".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Eighty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Ninety".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    
    99*"TwoHundredAnd".Length +
    "OneTwoThreeFourFiveSixSevenEightNineTenElevenTwelveThirteenFourteenFifteenSixteenSeventeenEighteenNineteen".Length +
    10*"Twenty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Thirty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Forty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Fifty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Sixty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Seventy".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Eighty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Ninety".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    
    99*"ThreeHundredAnd".Length +
    "OneTwoThreeFourFiveSixSevenEightNineTenElevenTwelveThirteenFourteenFifteenSixteenSeventeenEighteenNineteen".Length +
    10*"Twenty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Thirty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Forty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Fifty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Sixty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Seventy".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Eighty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Ninety".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    
    99*"FourHundredAnd".Length +
    "OneTwoThreeFourFiveSixSevenEightNineTenElevenTwelveThirteenFourteenFifteenSixteenSeventeenEighteenNineteen".Length +
    10*"Twenty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Thirty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Forty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Fifty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Sixty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Seventy".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Eighty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Ninety".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    
    99*"FiveHundredAnd".Length +
    "OneTwoThreeFourFiveSixSevenEightNineTenElevenTwelveThirteenFourteenFifteenSixteenSeventeenEighteenNineteen".Length +
    10*"Twenty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Thirty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Forty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Fifty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Sixty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Seventy".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Eighty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Ninety".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    
    99*"SixHundredAnd".Length +
    "OneTwoThreeFourFiveSixSevenEightNineTenElevenTwelveThirteenFourteenFifteenSixteenSeventeenEighteenNineteen".Length +
    10*"Twenty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Thirty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Forty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Fifty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Sixty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Seventy".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Eighty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Ninety".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    
    99*"SevenHundredAnd".Length +
    "OneTwoThreeFourFiveSixSevenEightNineTenElevenTwelveThirteenFourteenFifteenSixteenSeventeenEighteenNineteen".Length +
    10*"Twenty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Thirty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Forty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Fifty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Sixty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Seventy".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Eighty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Ninety".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    
    99*"EightHundredAnd".Length +
    "OneTwoThreeFourFiveSixSevenEightNineTenElevenTwelveThirteenFourteenFifteenSixteenSeventeenEighteenNineteen".Length +
    10*"Twenty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Thirty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Forty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Fifty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Sixty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Seventy".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Eighty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Ninety".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    
    99*"NineHundredAnd".Length +
    "OneTwoThreeFourFiveSixSevenEightNineTenElevenTwelveThirteenFourteenFifteenSixteenSeventeenEighteenNineteen".Length +
    10*"Twenty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Thirty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Forty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Fifty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Sixty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Seventy".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Eighty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Ninety".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    
    "OneTwoThreeFourFiveSixSevenEightNineTenElevenTwelveThirteenFourteenFifteenSixteenSeventeenEighteenNineteen".Length +
    10*"Twenty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Thirty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Forty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Fifty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Sixty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Seventy".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Eighty".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length +
    10*"Ninety".Length + "OneTwoThreeFourFiveSixSevenEightNine".Length
    
//simplified version of (c)
let problem17d =
    "OneThousand".Length +
    
    "OneHundred".Length +
    "TwoHundred".Length +
    "ThreeHundred".Length +
    "FourHundred".Length +
    "FiveHundred".Length +
    "SixHundred".Length +
    "SevenHundred".Length +
    "EightHundred".Length +
    "NineHundred".Length +
    
    99*("OneHundredAnd".Length +
        "TwoHundredAnd".Length +
        "ThreeHundredAnd".Length +
        "FourHundredAnd".Length +
        "FiveHundredAnd".Length +
        "SixHundredAnd".Length +
        "SevenHundredAnd".Length +
        "EightHundredAnd".Length +
        "NineHundredAnd".Length) +
    
    10*("OneTwoThreeFourFiveSixSevenEightNineTenElevenTwelveThirteenFourteenFifteenSixteenSeventeenEighteenNineteen".Length +
        10*("Twenty".Length + "Thirty".Length + "Forty".Length + "Fifty".Length + "Sixty".Length + "Seventy".Length + "Eighty".Length + "Ninety".Length) +
        8*("OneTwoThreeFourFiveSixSevenEightNine".Length))

//problem 18, (note: problem 67 is exactly the same, but cannot be solved using brute force)
//Find the maximum sum travelling from the top of the triangle to the base.
let tstr = "75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"

type cell = {value:int; mutable max:int}
let problem18 = 
    //let tlist = tstr.Split('\n') |> Array.toList |> List.map (fun row -> row.Split(' ') |> Array.toList |> List.map(fun s -> (s |> int, 0)))
    let tarr = Array2D.create 15 15 {value=0;max=0}
    tstr.Split('\n') |> Array.iteri (fun i row -> row.Split(' ') |> Array.iteri (fun j cell -> (tarr.[i,j] <- {value=(cell |> int); max=0})))

    //tarr.[0,0].max <- 75
    for i in 1..14 do
        for j in 0..i do
            if j - 1 > 0 then
                tarr.[i,j].max <- max (tarr.[i-1,j-1].value + tarr.[i-1,j-1].max) (tarr.[i-1,j].value + tarr.[i-1,j].max)
            else
                tarr.[i,j].max <- (tarr.[i-1,j].value + tarr.[i-1,j].max)
        
    let mutable largest = 0        
    for j in 0..14 do
        tarr.[14,j].max <- tarr.[14,j].value + tarr.[14,j].max
        if tarr.[14,j].max >  largest then
            largest <- tarr.[14,j].max
            
    largest

//a simplified version of (a)
let problem18b = 
    let tarr = Array2D.create 15 15 0
    tstr.Split('\n') |> Array.iteri (fun i row -> row.Split(' ') |> Array.iteri (fun j cell -> (tarr.[i,j] <- (cell |> int))))

    for i in 1..14 do
        for j in 0..i do
            if j - 1 > -1 then
                tarr.[i,j] <- (max tarr.[i-1,j-1] tarr.[i-1,j]) + tarr.[i,j]
            else
                tarr.[i,j] <- tarr.[i-1,j] + tarr.[i,j]
        
    let mutable largest = 0        
    for j in 0..14 do 
        largest <- max largest tarr.[14,j] 
           
    largest
    
//a simplified version of (b)
let problem18c = 
    //parse
    let tarr = Array2D.create 15 16 0 //make first column filled with zeros so no out of bounds checks required
    tstr.Split('\n') |> Array.iteri (fun i row -> row.Split(' ') |> Array.iteri (fun j cell -> (tarr.[i,j+1] <- (cell |> int))))

    //calc
    for i in 1..14 do //start with second row
        for j in 1..i+1 do //shift orientation to right
            tarr.[i,j] <- (max tarr.[i-1,j-1] tarr.[i-1,j]) + tarr.[i,j]

    //find largest        
    let mutable largest = 0        
    for j in 0..14 do 
        largest <- max largest tarr.[14,j] 
           
    largest
    
//functional, recursive version
let problem18d = 
    let tarr = Array2D.create 15 15 0
    tstr.Split('\n') |> Array.iteri (fun i row -> row.Split(' ') |> Array.iteri (fun j cell -> (tarr.[i,j] <- (cell |> int))))
    
    let rec find (row,col) = 
        if row = 14 then tarr.[row,col]
        else (max (find (row+1,col)) (find (row+1,col+1))) + tarr.[row,col]
    find (0,0)

let test = "
0
1  2
3  4  5
6  7  8  9
10 11 12 13 14
15 16 17 18 19 20"
//------
//0  -> (1,2) //x = i + 1, (x,x+1)
//1  -> (3,4) //i + 2
//2  -> (4,5)
//3  -> (6,7) //i + 3
//4  -> (7,8)
//5  -> (8,9)
//6  -> (10,11) //i + 4
//7  -> (11,12)
//8  -> (12,13)
//9  -> (13,14)
//10 -> (15,16) //i + 5
//11 -> (16,17)
//...


    
//problem 19
//How many Sundays fell on the first of the month during the twentieth century?
let problem19a = 
    let mutable cur = new System.DateTime(1901, 1, 1);
    let last = new System.DateTime(2000, 12, 31);
    let mutable count = 0
    while(cur <= last) do
        if cur.Day = 1 && cur.DayOfWeek = System.DayOfWeek.Sunday then
            count <- count + 1
            
        cur <- cur.AddDays(1.0)
            
    count
    
let problem19b =
    let last = System.DateTime(2000, 12, 31)
    let rec sundays cur count = 
        if cur > last then count
        elif cur.Day = 1 && cur.DayOfWeek = System.DayOfWeek.Sunday then sundays (cur.AddDays(1.0)) (count+1)
        else sundays (cur.AddDays(1.0)) count
    sundays (System.DateTime(1901, 1, 1)) 0

//problem 20: find the sum of the digits in 20!    
//let fact m = {1I..m} |> Seq.fold (*) (1I)
let problem20a =
    factorialI 100I |> string |> Seq.sumBy (fun c -> c |> string |> int)


//problem 21
//Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
//If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.
//
//For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
//
//Evaluate the sum of all the amicable numbers under 10000.

//let problem21a =
//    let pdivisors n = seq {for i in 1..n/2 do if n % i = 0 then yield i}
//    let d n = n |> pdivisors |> Seq.sum 
//    let rec build a alist =
//        if a = 10000 then alist
//        else
//            let b = d a
//            let a' = d b
//            if a' = a && a <> b then build (a+1) (a::alist)
//            else build (a+1) alist
//    (build 1 []) |> List.sum

let sigma n =
    factorize n
    |> Seq.groupBy id
    |> Seq.map (fun (f,s) -> (f, Seq.length s))
    |> Seq.map (fun (p,e) -> (pown p (e+1) - 1)/(p-1))
    |> Seq.fold (*) 1
    
let psigma n = sigma n - n
    
let problem21a =
    let rec build a alist =
        match (a,psigma a) with
        | (a,b) when a <> 1 && a <> b && (psigma b) = a -> build (a+1) (a::alist)
        | (9999,_) -> alist
        | _ -> build (a+1) alist
    (build 4 []) |> List.sum
    
let problem21b =
    let isAmicable a = 
        let b = psigma a
        a <> 1 && a <> b && (psigma b) = a    
    {4..9999} |> Seq.filter isAmicable |> Seq.sum
                                    
//Using names.txt  (right click and 'Save Link/Target As...'), a 46K text file containing 
//over five-thousand first names, begin by sorting it into alphabetical order. 
//Then working out the alphabetical value for each name, multiply this value by its
//alphabetical position in the list to obtain a name score.
//
//For example, when the list is sorted into alphabetical order, COLIN, which is
//worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain
//a score of 938 × 53 = 49714.
//
//What is the total of all the name scores in the file?

let problem22a =
    let text = System.IO.File.ReadAllText("names.txt")
    
    let names = text.Split([|',';'"'|], System.StringSplitOptions.RemoveEmptyEntries) 
    names |> Array.sortInPlace
    
    let chars = seq {for i in 65..90 do yield i |> char } |> Seq.toArray
    
    let score = ref 0
    for i in 0..names.Length-1 do
        let alphaValue = names.[i] |> Seq.sumBy (fun c -> System.Array.IndexOf(chars,c)+1)
        score := !score + ((i+1) * alphaValue)
    score
    
let problem22b =
    let text = System.IO.File.ReadAllText("names.txt")
    
    let names = text.Split([|',';'"'|], System.StringSplitOptions.RemoveEmptyEntries) 
    names |> Array.sortInPlace

    seq { for i in 0..names.Length-1 do 
              yield (i+1) * (names.[i] |> Seq.sumBy (fun c -> (int c) - 64)) }
    |> Seq.sum
    
//A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
//
//A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.
//
//As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.
//
//Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

let problem23a =
    let isAbundant n = (psigma n) > n
    let abundants = {1..28122} |> Seq.filter isAbundant |> Seq.toArray
    let cannots = System.Collections.Generic.List({1..28122})    
    for i in 0..(abundants.Length-1) do
        for j in i..(abundants.Length-1) do
            ignore (cannots.Remove(abundants.[i] + abundants.[j]))
    cannots |> Seq.sum
    
let problem23b =
    let isAbundant n = (psigma n) > n
    let abundants = {1..28122} |> Seq.filter isAbundant |> Seq.toArray
    let cannots = {1..28122} |> Seq.toArray
    for i in 0..(abundants.Length-1) do
        let rec removeAbundants j =
            let can = abundants.[i] + abundants.[j]
            if can > 28122 then ()
            else
                cannots.[can-1] <- 0 
                removeAbundants (j+1)
        removeAbundants i
    cannots |> Seq.sum
    
//What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
//problem 24
sw.Start()
let problem24a =
    let next (last:int[]) : int[] =
        let mutable i = last.Length-1
        while last.[i] < last.[i-1] do
            i <- (i-1)

        let it = last.[i-1]
        let mutable tail = last.[i..last.Length-1]
        let lub = tail |> Seq.filter (fun e -> e > it) |> Seq.min
        let lubi = last |> Array.findIndex (fun e -> e = lub)
        last.[i-1] <- lub
        last.[lubi] <-it
        tail <- last.[i..last.Length-1]
        
        tail |> Array.sortInPlace
        Array.blit tail 0 last i tail.Length
        last
        
        
    let mutable last = [|0;1;2;3;4;5;6;7;8;9|]
    let mutable i = 1
    while i < 1000000 do
        last <- next last
        i <- i+1
    last
sw.Stop()
printfn "%A" sw.ElapsedMilliseconds
sw.Reset()

sw.Start()
let problem24b =
    let next (last:int[]) : int[] =
        let rec findA ai =
            if last.[ai] >= last.[ai-1] then ((ai-1), last.[ai-1])
            else findA (ai-1)
        let (ai,a) = findA (last.Length-1)
        
        let tail = last.[ai+1..last.Length-1]
        
        let b = tail |> Seq.filter (fun e -> e > a) |> Seq.min
        let bi = tail |> Array.findIndex (fun e -> e = b)
        last.[ai] <- b
        tail.[bi] <- a
        
        tail |> Array.sortInPlace
        Array.blit tail 0 last (ai+1) tail.Length
        last
    
    let rec find last i =
        if i = 1000000 then last
        else find (next last) (i+1) 
    find [|0;1;2;3;4;5;6;7;8;9|] 1
sw.Stop()
printfn "%A" sw.ElapsedMilliseconds
sw.Reset()
    
sw.Start()
let problem24c =
    let next (prev:int[]) : int[] =
        let rec findA ai =
            if prev.[ai] >= prev.[ai-1] then ((ai-1), prev.[ai-1])
            else findA (ai-1)
        let (ai,a) = findA (prev.Length-1)
        
        let tail = prev.[ai+1..prev.Length-1]
        
        let mutable min = 0
        let mutable bi = 0
        let mutable b = tail.[0]
        for i in 0..tail.Length-1 do
            if tail.[i] > a && tail.[i] < b then
                bi <- i
                b <- tail.[i]
        
        prev.[ai] <- b
        tail.[bi] <- a
        
        tail |> Array.sortInPlace
        Array.blit tail 0 prev (ai+1) tail.Length
        prev
    
    let rec find prev i =
        if i = 1000000 then prev
        else find (next prev) (i+1) 
    find [|0;1;2;3;4;5;6;7;8;9|] 1
sw.Stop()
printfn "%A" sw.ElapsedMilliseconds
sw.Reset()

sw.Start()
let problem24d =
    let next (prev:int[]) =
        //step 1
        let rec findA i =
            if prev.[i] >= prev.[i-1] then ((i-1), prev.[i-1])
            else findA (i-1)
        let (ai,a) = findA (prev.Length-1)
        
        let tail = prev.[ai+1..prev.Length-1]
        
        //step 2
        let rec findB i (bi,b) =
            if i = tail.Length then (bi,b)
            elif tail.[i] > a && tail.[i] < b then findB (i+1) (i, tail.[i])
            else findB (i+1) (bi,b)
        let (bi,b) = findB 0 (0, tail.[0])
        
        prev.[ai] <- b
        tail.[bi] <- a
        
        //step 3
        tail |> Array.sortInPlace
        Array.blit tail 0 prev (ai+1) tail.Length
        prev
    
    let rec find prev i =
        if i = 1000000 then prev
        else find (next prev) (i+1) 
    find [|0;1;2;3;4;5;6;7;8;9|] 1
sw.Stop()
printfn "%A" sw.ElapsedMilliseconds
sw.Reset()

sw.Start()
//FASTEST
let problem24e =
    let next (perm:int[]) =
        //step 1
        let rec find i =
            if perm.[i] >= perm.[i-1] then i-1
            else find (i-1)
        let s = find (perm.Length-1)
        let s' = perm.[s]
        
        //step 2
        let tail = perm.[s+1..perm.Length-1]
        
        let rec find i imin =
            if i = tail.Length then imin
            elif tail.[i] > s' && tail.[i] < tail.[imin] then find (i+1) i
            else find (i+1) imin
        let t = find 0 0
        let t' = tail.[t]
        
        //swap
        perm.[s] <- t'
        tail.[t] <- s'
        
        //step 3
        tail |> Array.sortInPlace
        Array.blit tail 0 perm (s+1) tail.Length
        perm
    
    let rec find i perm =
        if i = 1000000 then perm
        else find (i+1) (next perm)
    find  1 [|0;1;2;3;4;5;6;7;8;9|]
sw.Stop()
printfn "%A" sw.ElapsedMilliseconds
sw.Reset()

sw.Start()
let problem24f =
    let next (perm:int[]) =
        //step 1
        let rec find i =
            if perm.[i] >= perm.[i-1] then i-1
            else find (i-1)
        let s = find (perm.Length-1)
        let s' = perm.[s]
        
        //step 2
        let rec find i imin =
            if i = perm.Length then imin
            elif perm.[i] > s' && perm.[i] < perm.[imin] then find (i+1) i
            else find (i+1) imin
        let t = find (s+1) (s+1)
        let t' = perm.[t]
        
        //swap
        perm.[s] <- t'
        perm.[t] <- s'
        
        //step 3
        perm |> Array.sortInPlaceWith 
            (fun a b -> 
                let i = System.Array.IndexOf(perm, a)
                let j = System.Array.IndexOf(perm, b)
                if i > s && j > s then a.CompareTo(b)
                else i-j)
        perm
    
    let rec find i perm =
        if i = 1000000 then perm
        else find (i+1) (next perm)
    find  1 [|0;1;2;3;4;5;6;7;8;9|]
sw.Stop()
printfn "%A" sw.ElapsedMilliseconds
sw.Reset()

sw.Start()
//essentially the same as FASTEST (e)
let problem24g =
    let advance (perm:int[]) =
        //Find the longest "tail" that is ordered in decreasing order ((s+1)..perm.Length-1).
        let rec find i =
            if perm.[i] >= perm.[i-1] then i-1
            else find (i-1)
        let s = find (perm.Length-1)
        let s' = perm.[s]
        
        //Change the number just before the tail (s') to the smallest number bigger than it in the tail (perm.[t]).
        let rec find i imin =
            if i = perm.Length then imin
            elif perm.[i] > s' && perm.[i] < perm.[imin] then find (i+1) i
            else find (i+1) imin
        let t = find (s+1) (s+1)
        
        perm.[s] <- perm.[t]
        perm.[t] <- s'
        
        //Sort the tail in increasing order.
        System.Array.Sort(perm, s+1, perm.Length - s - 1)
    
    let perm = [|0;1;2;3;4;5;6;7;8;9|]
    let rec find i =
        if i = 1000000 then perm
        else advance perm ; find (i+1) 
    find  1
sw.Stop()
printfn "%A" sw.ElapsedMilliseconds

//problem 25
//What is the first term in the Fibonacci sequence to contain 1000 digits?
//F_(n) = F_(n−1) + F_(n−2), where F_(1) = 1 and F_(2) = 1.

sw.Reset()
sw.Start()
let problem25a =
    let rec find n1 n2 term =
        let n0 = n1 + n2
        if (n0 |> string).Length = 1000 then term
        else find n0 n1 (term+1)
    find 1I 1I 3
sw.Stop()
printfn "%A" sw.ElapsedMilliseconds    

sw.Reset()
sw.Start()
let problem25b =
    let fibseq =    
        let rec fibseq n1 n2 = 
            seq { let n0 = n1 + n2 
                  yield n0
                  yield! fibseq n0 n1 }
        seq { yield 1I ; yield 1I ; yield! (fibseq 1I 1I) }                
    
    (fibseq |> Seq.findIndex (fun i -> (i |> string).Length = 1000)) + 1
sw.Stop()
printfn "%A" sw.ElapsedMilliseconds

//problem26
//Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.
open Microsoft.FSharp.Collections

let problem26c =     
    let cycleLength d =
        let rec cycleLength (steps:ResizeArray<int>) step =
            if d > step then 
                cycleLength steps (step*10)
            else 
                if steps.Contains(step) then
                    (d, steps.Count - steps.IndexOf(step))
                else
                    steps.Add(step)
                    let step = step - d*(step/d)
                    if step = 0 then
                        (d, 0)
                    else
                        cycleLength steps step
        cycleLength (ResizeArray<int>()) 1
    //on dual core, runs in 60ms vs. 90ms when parallelized, 
    //and twice as fast with larger ranges
    {2..999}
    |> PSeq.map cycleLength 
    |> PSeq.maxBy snd 
    |> fst
    
let problem27a =
    seq { for a in -999..999 do for b in -999..999 do yield (a,b) }
    |> PSeq.map(fun (a,b) -> (a*b, Seq.initInfinite id
                                   |> Seq.takeWhile (fun n -> n*n + a*n + b |> isPrime)
                                   |> Seq.length))
    |> PSeq.maxBy snd
    |> fst
    
let problem28a =
    let diags upperdim =
        let rec diags dim prev =
            seq { let prev = (dim-1) + prev
                  yield prev
                  let prev = (dim-1) + prev
                  yield prev
                  let prev = (dim-1) + prev
                  yield prev
                  let prev = (dim-1) + prev
                  yield prev
                  if dim <> upperdim then 
                    yield! diags (dim+2) prev }
        diags 3 1
    (diags 1001 |> Seq.sum) + 1 //add center 1
                
let problem28b =
    let diags upperdim =
        let rec diags dim prev =
            seq { let next i = i*(dim-1) + prev
                  yield! {1..4} |> Seq.map next
                  if dim <> upperdim then 
                    yield! diags (dim+2) (next 4) }
        diags 3 1
    (diags 1001 |> Seq.sum) + 1 //add center 1

let problem28c =
    let diags =
        let rec diags dim prev =
            seq { let next i = (dim, i*(dim-1) + prev)
                  yield! {1..4} |> Seq.map next
                  yield! diags (dim+2) (next 4 |> snd) }
        seq { yield (1,1) ; yield! diags 3 1 }
    diags |> Seq.takeWhile(fun (dim,_)  -> dim <= 1001) |> Seq.sumBy snd
    
let problem28d =
    let diags =
        let rec diags dim prev =
            seq { let next i = i*(dim-1) + prev
                  yield! {1..4} |> Seq.map next
                  yield! diags (dim+2) (next 4) }
        seq { yield 1 ; yield! diags 3 1 }
    diags |> Seq.take((4*500)+1) |> Seq.sum
    
//How many distinct terms are in the sequence generated by a^(b) for 2 ≤ a  ≤ 100 and 2 ≤ b  ≤ 100?
let problem29a =
    seq {for a in 2I..100I do for b in 2..100 do yield bigint.Pow(a,b)}
    |> Seq.distinct 
    |> Seq.length
    
//We must find an upper limit for numbers to search.  As the length of a number increases,
//9**5 is the greatest additional contribution a digit can make to the sum of 5th power digits.  
//Meanwhile, the number itself is increasing by powers of 10, which will eventually overcome
//the rate of increase given summing 5th powers of digits.  So, when we find that the sum of 5th power digits
//of 9999999 (of length 7) equals 413343 (of length 6), we can be sure that no higher number can produce
//5th power digit sum of sufficient length.  (a non-rigorous proof, I know).
//
let problem30a =
    let powdigsum n e = n |> string |> Seq.map(string >> int32) |> Seq.sumBy(fun i -> pown i e)
    {2..9999999}
    |> Seq.filter(fun n -> (powdigsum n 5) = n)
    |> Seq.sum

//this one generates permutations instead of combinations
let problem31a = 
    let coins = [1;2;3;4]
    let amt = 4
    let rec perms combo =
        seq { 
            let sum = List.sum combo
            if sum = amt then 
                yield combo
            elif sum < amt then 
                yield! coins 
                       |> Seq.collect (fun c -> perms (c::combo))
        }
    perms [] |> Seq.toList

let problem31c =
    let combinations amt coins = //produce all ascending combinations starting with start
        let rec combinations combo =
            seq{ let sum = List.sum combo
                 if sum = amt then 
                     yield combo
                 elif sum < amt then 
                     yield! coins 
                            |> Seq.filter ((<=) combo.Head) //to generate combinations instead permuations
                            |> Seq.collect (fun c -> combinations (c::combo)) }
        seq {for start in coins do yield! combinations [start]}  //concat all ascending combinations for each start
    combinations 200 [1;2;5;10;20;50;100;200] |> Seq.length
    
let problem32b =
    let digits n = n |> string |> Seq.map (fun c -> c |> string |> int) |> Seq.toList
    
    let hasDistinctDigits_no0 digs = 
        not (digs |> Seq.exists ((=) 0)) && (List.length digs) = (digs |> Seq.distinct |> Seq.length)
    
    let panDigital_1to9 mn mr p =
        let concatedDigits = List.concat [digits mn; digits mr; digits p]
        (List.length concatedDigits) = 9 && hasDistinctDigits_no0 concatedDigits
    
    {1234..98765} //product must be between 4 and 5 digits
    |> Seq.map (
        fun p -> 
            let digs = digits p
            if not (hasDistinctDigits_no0 digs) then None
            else
                let sr = sqrtn p
                let rec loop mn =
                    if mn > sr then None
                    elif p % mn = 0 then
                        let mr = p/mn
                        if panDigital_1to9 mn mr p then Some(mn, mr, p) 
                        else loop (mn + 1)
                    else loop (mn + 1)
                loop 2)            
    |> Seq.filter Option.isSome
    |> Seq.sumBy (fun (Some(_,_,p)) -> p)
    
let problem32c =
    let hasDistinctPositiveDigits (nstr:string) = 
        not (nstr.Contains("0")) && nstr.Length = (nstr |> Seq.distinct |> Seq.length)
    
    let isPositivePanDigitalSet mnstr mrstr pstr =
        let concatedDigitStr:string = mnstr + mrstr + pstr
        concatedDigitStr.Length = 9 && hasDistinctPositiveDigits concatedDigitStr
        
    let findPositivePanDigitalSet p =
        let pstr = p |> string
        if not (hasDistinctPositiveDigits pstr) then None
        else
            let sr = sqrtn p
            let rec loop mn =
                if mn > sr then None
                elif p % mn = 0 then
                    let mr = p/mn
                    if isPositivePanDigitalSet (mn |> string) (mr |> string) pstr then 
                        Some(mn, mr, p) 
                    else loop (mn + 1)
                else loop (mn + 1)
            loop 2
    
    {1234..98765} //product must be between 4 and 5 digits
    |> Seq.map findPositivePanDigitalSet
    |> Seq.filter Option.isSome
    |> Seq.sumBy (fun (Some(_,_,p)) -> p)
    
let problem33a =
    let unorthodoxPairs =
        [for numer = 10 to 99 do
            for denom = (numer+1) to 99 do
                let reduced = (numer |> float)/(denom |> float)
                let digitPair n = //decompose digit list of n into a pair
                    let nDigits = n |> string |> Seq.map (string >> float) |> Seq.toArray
                    (nDigits.[0], nDigits.[1])
                let (a,b) = digitPair numer
                let (c,d) = digitPair denom
                let isUnorthodox w x y z = (w/x) = 1. && (y/z) = reduced
                if isUnorthodox a c b d || 
                   isUnorthodox a d b c || 
                   isUnorthodox b d a c || 
                   isUnorthodox b c a d then yield (numer, denom)]
    
    let product = unorthodoxPairs |> List.fold (fun (w,x) (y,z) -> (w*y,x*z)) (1,1)
    snd product / (gcd (fst product) (snd product)) 

//answer is 100    
let problem33b =
    let unorthodoxPairs =
        [for numer = 10 to 99 do
            for denom = (numer+1) to 99 do
                let reduced = (numer |> float)/(denom |> float)
                let digitPair n = //decompose digit list of n into a pair
                    let nDigits = n |> string |> Seq.map (string >> float) |> Seq.toArray
                    (nDigits.[0], nDigits.[1])
                let (a,b) = digitPair numer
                let (c,d) = digitPair denom
                let isUnorthodox w x y z = (w/x) = 1. && (y/z) = reduced
                if isUnorthodox a c b d || 
                   isUnorthodox a d b c || 
                   isUnorthodox b d a c || 
                   isUnorthodox b c a d then yield BigRational.Parse(sprintf "%d/%d" numer denom)]
    
    let product = unorthodoxPairs |> List.fold (*) 1N
    product.Denominator
    
let problem33c =
    let unorthodoxPairs =
        [for numer = 10. to 99. do
            for denom = (numer+1.) to 99. do                
                let digitPair n =
                    let tensPlace = System.Math.Truncate(n/10.)
                    (tensPlace, n-tensPlace*10.)    

                let (a,b) = digitPair numer
                let (c,d) = digitPair denom

                let isUnorthodox w x y z = (w/x) = 1. && (y/z) = numer/denom
                if isUnorthodox a c b d || 
                   isUnorthodox a d b c || 
                   isUnorthodox b d a c || 
                   isUnorthodox b c a d then yield BigRational.Parse(sprintf "%.0f/%.0f" numer denom)]
    
    let product = unorthodoxPairs |> List.fold (*) 1N
    product.Denominator
    
//The upper bound is 2540160, since (factorial 9)*7 = 2540160 and
//(factorial 9)^x grows faster than 10^x for x >= 7.

let problem34a = 
    [for i in 3..2540160 do
        if i = (i |> string |> Seq.map (string >> int) |> Seq.sumBy factorial) then
            yield i]
    |> Seq.sum
 
let problem34b =   
    {3..2540160}
    |> Seq.filter (fun i -> i = (i |> string |> Seq.map (string >> int) |> Seq.sumBy factorial))
    |> Seq.sum
        
let problem34c =
    {3..2540160}
    |> Seq.filter (fun i -> i = (i |> Digits.fromInt |> Seq.sumBy factorial))
    |> Seq.sum

//How many circular primes are there below one million?    
let problem35a = //"a" but came after version "b" and is superior
    let isCircularPrime n =
        if not (isPrime n) then false
        else 
            let digs = n |> string |> Seq.map string
            let rec loop i = 
                if i = (Seq.length digs)-1 then true
                else
                    let i = i + 1
                    let r = //rotate by i
                        Seq.append 
                            (digs |> Seq.skip i) 
                            (digs |> Seq.take i)
                        |> Seq.fold (+) "" //could use StringBuilder
                        |> int
                    if isPrime r then loop i
                    else
                        false
            loop 0

    //we are given that there are 13 such primes below 100
    ({101..2..999999}
    |> Seq.filter isCircularPrime
    |> Seq.length) + 13
    
//let ndigs = digits n |> Seq.map string |> Seq.toArray
open System.Collections.Generic
let problem35b =
    let isCircularPrime n =
        if not (isPrime n) then false
        else 
            let rotate (digs:string[]) n = 
                System.String.Concat(digs.[n..digs.Length-1]) +
                System.String.Concat(digs.[0..n-1])
                |> int
                
            let ndigs = n |> Digits.fromInt |> Seq.map (string) |> Seq.toArray
            let rec loop i = 
                if i = ndigs.Length-1 then true
                else
                    let i = i + 1
                    let r = rotate ndigs i
                    if isPrime r then loop i
                    else
                        false
            loop 0

    //we are given that there are 13 such primes below 100
    ({101..2..999999}
    |> Seq.filter isCircularPrime
    |> Seq.length) + 13        
    
//final version based on "a" but using Digits module
let problem35c = 
    let isCircularPrime n =
        if not (isPrime n) then false
        else 
            let digs = n |> Digits.fromInt
            let rec loop i = 
                if i = (Seq.length digs)-1 then true
                else
                    let i = i + 1
                    let r = //rotate by i
                        Seq.append 
                            (digs |> Seq.skip i) 
                            (digs |> Seq.take i)
                        |> Digits.toInt
                    if isPrime r then loop i
                    else
                        false
            loop 0

    //we are given that there are 13 such primes below 100
    ({101..2..999999}
    |> Seq.filter isCircularPrime
    |> Seq.length) + 13
    
//Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
let problem36a =
    let isPalindrome s = //from problem 4
        let rec isPalindrome (s:string) i j =
            if i > j then true 
            elif  s.[i] <> s.[j] then false
            else isPalindrome s (i+1) (j-1)
        isPalindrome s 0 (s.Length-1)

    {1..999999}
    |> Seq.filter (fun n -> (isPalindrome (n|>string)) && (isPalindrome (System.Convert.ToString(n, 2))))
    |> Seq.sum
    
let problem37a =
    let isTruncatablePrime n =
        if n |> isPrime |> not then false
        else
            let digs = n |> Digits.fromInt
            let truncations =
                seq { for i in 1..(Seq.length digs)-1 -> 
                          digs |> Seq.take i |> Digits.toInt
                      for i in 1..(Seq.length digs)-1 -> 
                          digs |> Seq.skip i |> Digits.toInt }
            Seq.forall isPrime truncations
            
    let rec odds n = seq {yield n; yield! odds (n+2)}
    odds 11 //skip the single digit primes
    |> Seq.filter isTruncatablePrime
    |> Seq.take 11 //we are given there are only 11
    |> Seq.sum
    
//What is the largest 1 to 9 pandigital 9-digit number that can be
// formed as the concatenated product of an integer with (1,2, ... , n)
// where n > 1?
//note: clearly n <=9 and p <= 987654321
let problem38a =
    let isPosPd (nstr:string) = //we know our chars are digits
        let len = nstr |> Seq.length
        len = 9 
        && (nstr.Contains("0") |> not) 
        && len = (nstr |> Seq.distinct |> Seq.length)
    
    let tryFind n =
        let rec loop i =
            let prodConcats = (seq {for j in 1..i -> n*j |> string }) |> Seq.fold (+) ""
            if isPosPd prodConcats then Some(prodConcats |> int)
            elif i < 9 then loop (i+1)
            else None
        loop 2 //start with 2 since n > 1
        
    {1..9999} //since n > 1, 9999 is clearly an upper limit
    |> PSeq.choose tryFind
    |> PSeq.max
    
    
//        seq {for i in 2..9 ->
//                Seq.concat 
//                    (seq {for j in 1..i -> 
//                             n*j |> string }) }
//        |> Seq.exists isPositivePandigital

//If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.
//
//{20,48,52}, {24,45,51}, {30,40,50}
//
//For which value of p ≤ 1000, is the number of solutions maximised?
let problem39a =
    let hypotenuse a b = sqrtF ((pown a 2) + (pown b 2) |> float)    
    
    {1..1000}
    |> PSeq.map (
            fun p ->
                let mutable count = 0
                for a in 1..p do
                    for b in a..p do
                    let c = hypotenuse a b
                    if c = truncF c && (a+b+(c|>int)) = p then
                        count <- count + 1                
                (p, count))
    |> PSeq.maxBy snd 
    |> fst

let problem39c =
    let countTriplets p = 
        let mutable count = 0
        for a in 1..p do
            for b in a..p do
                let c = p - (a + b) |> float
                let c' = a*a + b*b |> float |> sqrtF
                if c = c' then
                    count <- count + 1
        count
        
    {1..1000}
    |> PSeq.maxBy countTriplets
    
    
let problem40a =
    let sb = System.Text.StringBuilder()
    let mutable n = 1
    while sb.Length <= 1000000 do
        sb.Append(n) |> ignore
        n <- n+1
        
    let mutable prod = 1
    for i in 0..6 do
        prod <- prod * ((sb.[(pown 10 i)-1] |> int) - 48) 
        
    prod
           
let problem41a = 
    let isPandigital n =
        let dlist = n |> Digits.fromInt |> List.ofSeq
        (List.sort dlist) = [1..List.length dlist]
        
    let rec loop n =
        if isPrime n && isPandigital n then n
        else loop (n-2)
    loop 987654321
    
//let problem41b =
//    let rec loop n =
//        let digs = [|n..(-1)..1|]
//        let last = [|1..n|]
//        
//        let rec tryFind() = //advance digs with each failed iteration
//            let value = digs |> Seq.map(string) |> Seq.fold (+) "" |> int
//            if value |> isPrime then Some(value)
//            elif digs = last then None
//            else permuteDesc digs ; tryFind()
//            
//        match tryFind() with
//        | Some(value) -> value
//        | None -> loop (n-1)
//        
//    loop 9
open Microsoft.FSharp.Core.Operators    
let problem41c =    
    let rec loop n =
        let maybe = 
            {n..(-1)..1}
            |> lexPermsDesc
            |> Seq.map Digits.toInt
            |> Seq.tryFind isPrime
            
        match maybe with
        | Some(value) -> value
        | None -> loop (n-1)
        
    loop 9
    
let problem42a =
    let triangle n = (n*(n+1))/2
    let tseq = Seq.initInfinite (fun i -> triangle (i+1)) |> Seq.cache
    
    let wordValue (s:string) = s |> Seq.sumBy (fun c -> (c |> int)-64)
    
    let text = System.IO.File.ReadAllText("words.txt")
    let words = text.Split([|',';'"'|], System.StringSplitOptions.RemoveEmptyEntries) |> Seq.readonly
    
    let isTriangleWord w =
        let wv = wordValue w
        tseq
        |> Seq.takeWhile (fun t -> t <= wv)
        |> Seq.exists ((=) wv)
        
    words
    |> Seq.filter isTriangleWord
    |> Seq.length


let problem43a =  
    let hasProperty p = 
        let ddd = p |> Seq.skip 1
                    |> Seq.windowed 3
                    |> Seq.map Digits.toInt 
        
        Seq.zip ddd [2;3;5;7;11;13;17]
        |> Seq.forall (fun (a,b) -> a % b = 0)
            
    lexPermsAsc [0;1;2;3;4;5;6;7;8;9]
    |> Seq.filter hasProperty
    |> Seq.sumBy Digits.toInt64

let problem44a =
    let p n = n*(3*n-1)/2 //p = (3n^2-n)/2
    let pinv n = (1 + sqrtn(24*n + 1))/6 //inverse for positive range of p
    let isPentagonal t = t = (p (pinv t))
    
    let testPair p1 p2 = 
        (isPentagonal (p1-p2)) && (isPentagonal (p1+p2))
            
    let rec loop n minDiff =
        let pn = p n
        if ((p (n+1)) - pn) > minDiff then minDiff
        else
            let rec find m = 
                let pm = p m
                if m = 0 then None
                elif testPair pn pm then Some(pm)
                else find (m-1)
            match find (n-1) with
            | Some(pm) -> loop (n+1) (min (pn-pm) minDiff) //take the first pair
            | None -> loop (n+1) minDiff
    loop 2 System.Int32.MaxValue
    
    //suppose pi - pj, where i>j has the property, then
    //if p(i+1) - pi > (pi - pj), 
    
    
    //The derivative of p is linear (p' = (3n^s - n)/2), so p is strickly increasing.
    //Therefore, if p(i-1) - p(i) > p(s) - p(t) where i > s > t and p(s) and p(t) are
    //pentagonal numbers for which their sum and difference is pentaganol
    
    //The derivative of p is linear (p' = (3n^s - n)/2), so p is strickly increasing. 
    //Therefore, if p(i-1) - p(i) > min {p(s) - p(t) where i > s > t and p(s) and p(t) 
    //are pentagonal numbers for which their sum and difference is pentaganol} then the 
    //right-hand side is minimized.
    
//It can be verified that T_(285) = P_(165) = H_(143) = 40755.    
//After 40755, what is the next triangle number that is also pentagonal and hexagonal?
let problem45a =
    let pent n = n*(3L*n-1L)/2L
    let pentInv n = (1L + sqrtL(24L*n + 1L))/6L
    let isPent n = (n = (pent (pentInv n)))
    
    let hex n = n*(2L*n-1L)
    let hexInv n = (1L + sqrtL(8L*n + 1L))/4L
    let isHex n = (n = (hex (hexInv n)))
    
    let tri n = n*(n+1L)/2L
    let rec triSeq n = seq {yield tri n; yield! triSeq (n+1L)}
    
    triSeq 286L
    |> Seq.find (fun n -> isPent n && isHex n)
    

//What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?
//answer 5777
//version a needs to be refectored: the nested loop in hasForm requires break / continue
//or will take very long time to compute.
let problem46a = 
    let form p s = p + 2*(s*s)
    let oddComposites = 
        let rec oddComposites n = 
            seq {
                if n |> isPrime |> not then
                    yield n

                yield! oddComposites (n+2)
            }
        oddComposites 9
    
    //cached for performance
    let primes = Seq.initInfinite id |> Seq.filter isPrime |> Seq.cache

    //exclusive
    let primesUpto n = primes |> Seq.takeWhile ((>=) n)

    //refactor!
    let hasForm n = 
        seq {
            for p in primesUpto n do
                let r =
                    {1..n-1} 
                    |> Seq.map (fun s -> form p s)
                    |> Seq.find (fun r -> (r = n) || (r > n))
                
                if r = n then yield true
        } |> Seq.length > 0


    oddComposites
    |> Seq.find (hasForm>>not)

let problem46b = 
    let form p s = p + 2*(s*s)

    //making sure to exclude 1    
    let oddComposites = 
        Seq.initInfinite id 
        |> Seq.filter (fun i -> i <> 1 && i%2=1 && isPrime i |> not)
    
    //cached for performance
    let primes = 
        Seq.initInfinite id 
        |> Seq.filter isPrime 
        |> Seq.cache

    //exclusive
    let primesUpto n = 
        primes 
        |> Seq.takeWhile ((>=) n)

    //think nested loops
    let hasForm n = 
        primesUpto n
        |> Seq.exists 
            (fun p ->
                ({1..n-1} 
                |> Seq.map (fun s -> form p s)
                |> Seq.find (fun r -> (r = n) || (r > n))) = n)

    oddComposites
    |> Seq.find (hasForm>>not)

//Find the first four consecutive integers to have four distinct primes factors. What is the first of these numbers? 
let problem47a =
    (Seq.initInfinite (fun i -> let n = i+3 in (n, cfactorize n)) //starting with 0+3=2
    |> Seq.windowed 4
    |> Seq.find (fun arr -> arr |> Seq.forall (fun (_,lst) -> lst |> List.length = 4))).[0]
    |> fst

//only modest performance increase
let problem47b =
    (Seq.initInfinite (fun i -> factorize (i+3)) //starting with 0+3=2
    |> Seq.windowed 4
    |> Seq.find (fun arr -> arr |> Seq.forall (fun lst -> lst |> Seq.distinct |> Seq.length = 4))).[0]
    |> List.fold (*) 1

//Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000
let problem48a =
    let digits =
        {1..1000}
        |> Seq.sumBy (fun n -> BigInteger.Pow(BigInteger(n), n))
        |> Digits.fromBigInt
        |> Seq.toArray

    Array.sub digits (digits.Length - 10) 10
    |> Digits.toInt64 //just makes it easier to copy and paste result

//let problem49a =
//    let fourDigitPrimes =
//        {1000..9999}
//        |> Seq.filter isPrime
//
//    let primePerms =
//        fourDigitPrimes
//        |> Seq.map (fun p -> Digits.fromInt p)
//        |> Seq.filter (fun perms -> (perms |> Seq.length) = (perms |> Seq.distinct |> Seq.length))
//        |> Seq.map (fun perms -> perms |> Digits.toInt)
//        |> Seq.filter (fun perms -> perms |> Seq.forall isPrime)
//
//    primePerms |> Seq.length

//let problem49a =
//    let possibleStartingPrimes =
//        {1000..9999}
//        |> Seq.filter (fun n -> isPrime n)
//
//    let arithmeticSeq =
//        possibleStartingPrimes
//        |> Seq.map (fun p -> possibleStartingPrimes |> Seq.filter (fun psp -> psp > p) |> Seq.map (fun psp -> (p, psp, psp-p)))
//
//    let arithmeticSeqGroup =
//        arithmeticSeq
//        |> Seq.groupBy (fun s -> 

let problem49a =
    {100000000000L..999999999999L}
    |> Seq.find (fun n -> 
        let res = n |> string |> Digits.uncheckedParse |> Seq.toArray
        let a = res.[0..3]
        let b = res.[4..7]
        let c = res.[8..11]
        
        let setA = Set(a)
        let setB = Set(b)
        printn n
        if setA = setB && setB = Set(c) then //are permutations of each other
            let a = a |> Digits.toInt
            let b = b |> Digits.toInt
            let c = c |> Digits.toInt
            if b-a = c-b then //is arithmetic seq
                if a |> isPrime && b |> isPrime && c |> isPrime && a <> 1487 then true
                else false
            else false
        else false
    )

let problem49b =
    let fourDigitPrimes =
        {1000..9999}
        |> Seq.filter isPrime
        |> Seq.toArray

    let arithmeticTriples = seq {
        for i in {0..fourDigitPrimes.Length-1} do
            for j in {i+1..fourDigitPrimes.Length-1} do
                for k in {j+1..fourDigitPrimes.Length-1} do
                    let a = fourDigitPrimes.[i]
                    let b = fourDigitPrimes.[j]
                    let c = fourDigitPrimes.[k]
                    //is arithmetic seq other than given in question
                    if b-a = c-b && a <> 1487 then 
                        yield (a,b,c)
    }
    
    let arePerms (a,b,c) = 
        let setA = Set(Digits.fromInt a)
        let setB = Set(Digits.fromInt b)
        setA = setB && setB = Set(Digits.fromInt c)
        
    arithmeticTriples
    |> Seq.find arePerms
    |> (fun (a,b,c) ->
            [Digits.fromInt a; Digits.fromInt b; Digits.fromInt c] 
            |> Seq.concat 
            |> Digits.toInt64)

let problem49c =
    let fourDigitPrimes =
        {1000..9999}
        |> Seq.filter isPrime
        |> Seq.toArray

    let arithmeticTriples = seq {
        for i in {0..fourDigitPrimes.Length-1} do
            for j in {i+1..fourDigitPrimes.Length-1} do
                for k in {j+1..fourDigitPrimes.Length-1} do
                    let a = fourDigitPrimes.[i]
                    let b = fourDigitPrimes.[j]
                    let c = fourDigitPrimes.[k]
                    //is arithmetic seq other than given in question
                    if b-a = c-b && a <> 1487 then 
                        yield (Digits.fromInt a,Digits.fromInt b,Digits.fromInt c)
    }

    let arePerms (a,b,c) = 
        let setA, setB = Set(a), Set(b)
        setA = setB && setB = Set(c)
    
    arithmeticTriples
    |> Seq.find arePerms
    |> (fun (a,b,c) -> [a;b;c] |> Seq.concat |> Digits.toInt64)

//Which prime, below one-million, can be written as the sum of the most consecutive primes?
let problem50b =
    let primesArr = primes |> Seq.takeWhile ((>=)1000000) |> Seq.toArray
    let count p =
        let rec count p cur curMax =
            let start = primesArr.[cur]
            if start >= p/2 then curMax
            else
                let rec consecutiveSum i sum =
                    if sum >= p then (i,sum)
                    else consecutiveSum (i+1) (sum + primesArr.[i])

                let i,sum = consecutiveSum cur 0            
                let curMax = max curMax (if sum = p then i-cur else 0)
                count p (cur+1) curMax
        count p 0 0

    primesArr
    |> Seq.maxBy count