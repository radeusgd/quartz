all:
	# parser should be regenerated, but I disabled it and distribute the parser code, because happy @ students seems to be some older version and there are issues, so I pre-generated it locally
	# ./generate_parser.sh
	/home/students/inf/PUBLIC/MRJP/Stack/stack install --local-bin-path .
	mv quartzlang-exe interpreter
