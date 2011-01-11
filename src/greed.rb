def score(dice)	
  compute_singles(dice) + compute_triples(dice)  
end

def compute_singles(diceRoll)
	result = 0
	diceRoll.each do |roll|
		result+= single_value(roll)
  	end
  	result
end

SINGLE_VALUES = { 1 => 100 , 5 => 50}
def single_value(roll)
	return SINGLE_VALUES[roll] if SINGLE_VALUES.keys.include?(roll)
	0
end 

def compute_triples(diceRoll)
	result   = 0
	asString = diceRoll.sort.to_s
	for points in (1..6) do	
	  ocurrences=diceRoll.count(points)
	  if ( ocurrences>=3 )
	      result	  +=  calculate_triple_score(points)
	  end
  	end
  	result
end 

TRIPLE_FACTOR               = 100
SPECIAL_TRIPLE_SCORES       = {1=>1000}

def calculate_triple_score(points)
	tripleScore = ( points * TRIPLE_FACTOR ) 
	if SPECIAL_TRIPLE_SCORES.keys.include?(points)
		tripleScore = SPECIAL_TRIPLE_SCORES[points] 
	end
	tripleScore - score_of_triple_as_singles(points)
end 

def score_of_triple_as_singles(points)
	single_value(points) * 3
end
