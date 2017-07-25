from sys import *
from os import system

tokens = []

def open_file(filename):
    data = open(filename, "r").read()
    return data

def lex(filecontents):
    filecontents = list(filecontents)
    token = ""
    string = ""
    state = 0
    num_found = False
    binding_found = False
    negative_found = False
    bound_given = False
    end_found = False
    num_to_append = ""
    first_num = True
    for char in filecontents:
        token += char
        if token == ";" or token == ")":
            token = ""
            end_found = True
            num_to_append = ""
        if token == " ":
            if state == 0:
                token = ""
            else:
                token = " "
        elif token == "\n" or token == "<EOF>":
            token = ""
        elif token == "print":
            tokens.append(token)
            token = ""
        elif token == "=>":
            binding_found = True
            first_num = True
        elif token == "-":
            negative_found = True
            token = ""
        elif token in ['0','1','2','3','4','5','6','7','8','9'] or negative_found:
            if first_num:
                tokens.append("Bind")
            else:
                first_num = False
            num_found = True
            if negative_found:
                num_to_append += "-" + token
                negative_found = False
            else:
                num_to_append += token
            token = ""
        elif token == "(\"" or token == "\"":
            if state == 0: #We found the beginning of the string
                state = 1
            elif state == 1: #We found the end of the string
                tokens.append(string[1:] + "\"")
                string = ""
                state = 0
                token = ""
        elif state == 1: #Adding the middle of the string
            string += token
            token = ""
        elif binding_found and char != " ":
            num_to_append += token
            tokens.append(num_to_append)
            binding_found = False
            num_found = False
            token = ""
            num_to_append = ""
    print(tokens)
    return tokens

def parse(g):
    variables = []
    file = open("eugene.rs", 'w') #}
    file.write("")                #- Clearing the file
    file.close()                  #} 
    file = open("eugene.rs", 'a')
    file.write("fn main() {\n")
    for x in g:
        if x[0] == "print":
            file.write("\t" + "println!(" + x[1] + ");" + "\n")
        if x[0] == "Bind":
            split = x[1].split(" ")
            if split[len(split)-1] != "Bind":
                file.write("\t" + "let mut " + split[len(split)-1] + " = " + split[0][:-2] + ";" + "\n")
                variables.append((split[len(split)-1], split[0][:-2]))
    file.write("}")
    file.close()

def compile(output_name):
    if output_name == "":
        output_name = "Carrie"
    system("rustc eugene.rs -o " + output_name)
    system("rm eugene.rs")

def group(unsorted):
    sorted = []
    even = False
    i = 0
    while(i < len(unsorted)):
        if unsorted[i+1] == "Bind" and not even:
            unsorted.pop(i)
        else:
            even
        sorted.append((unsorted[i], unsorted[i+1]))
        i += 2
    return sorted

def run():
    data = open_file(argv[1])
    lexed_data = lex(data)
    g_data = group(lexed_data)
    eugene = parse(g_data)
    
    #output_name = input("λ: ")
    #compile(output_name)

run()
