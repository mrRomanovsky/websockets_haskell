#Simple remote calculations service: 

Clients request expressions' calculationsпросят сервер вычислить выражение (для начала содержащее только одну арифметическую операцию +, -, *, / ) 

Server returns the results

#Protocol:

##Request: 
CALC<spaces>[operation]<spaces>[operand 1]<spaces>[operand 2]<spaces + optional newline>

##Response : 
ОК [result] \n (everything is fine)
ERR \n (something is wrong with the request)