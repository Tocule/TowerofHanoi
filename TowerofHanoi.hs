data Tower = First|Middle|Last deriving (Show,Eq)


f :: Tower
m :: Tower
l :: Tower
f=First
m=Middle
l=Last
type Step=(Integer,(Tower,Tower))

solution :: Integer -> [Step]

solution 0 = []
solution n = myHanoi n First Middle Last

myHanoi :: Integer -> Tower -> Tower -> Tower -> [Step]


myHanoi 1 f m l = [(1,(f,l))]
myHanoi n f m l =
		let 
		    step1 = myHanoi (n-1) f l m 
	            step2 = (n,(f,l))
	            step3 = myHanoi (n-1) l m f
    		 in step1 ++ [step2] ++ step3
    

