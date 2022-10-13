import os
import shutil

if __name__ == "__main__": 
    shutil.rmtree("lib/gen")
    os.mkdir("tmp")

    os.chdir("tree-sitter-prowl")
    os.system("tree-sitter generate")
    os.chdir("..")

    os.system("ocaml-tree-sitter gen --out-dir=tmp prowl tree-sitter-prowl/src/grammar.json")
    shutil.copytree("tmp/lib", "lib/gen")

    os.system("dune build")
