#!/bin/bash
# Script to add :mkdirp yes to all tangle directives that don't have it

# First the basics.org file
sed -i 's/#+begin_src scheme :tangle "\([^"]*\)"$/#+begin_src scheme :tangle "\1" :mkdirp yes/g' examples/basics.org
sed -i 's/#+begin_src scheme :tangle "\([^"]*\)" :noweb yes$/#+begin_src scheme :tangle "\1" :noweb yes :mkdirp yes/g' examples/basics.org
sed -i 's/#+begin_src scheme :tangle \([^ ]*\) :noweb yes$/#+begin_src scheme :tangle \1 :noweb yes :mkdirp yes/g' examples/basics.org
sed -i 's/#+begin_src mermaid :file \([^ ]*\) :tangle \([^ ]*\)$/#+begin_src mermaid :file \1 :tangle \2 :mkdirp yes/g' examples/basics.org

# Now the functors.org file
sed -i 's/#+begin_src scheme :tangle "\([^"]*\)"$/#+begin_src scheme :tangle "\1" :mkdirp yes/g' examples/functors.org
sed -i 's/#+begin_src scheme :tangle "\([^"]*\)" :noweb yes$/#+begin_src scheme :tangle "\1" :noweb yes :mkdirp yes/g' examples/functors.org
sed -i 's/#+begin_src scheme :tangle \([^ ]*\) :noweb yes$/#+begin_src scheme :tangle \1 :noweb yes :mkdirp yes/g' examples/functors.org
sed -i 's/#+begin_src mermaid :file \([^ ]*\) :tangle \([^ ]*\)$/#+begin_src mermaid :file \1 :tangle \2 :mkdirp yes/g' examples/functors.org

# And finally modular-systems-and-symmetry.org
sed -i 's/#+begin_src guile :tangle \([^ ]*\)$/#+begin_src guile :tangle \1 :mkdirp yes/g' examples/modular-systems-and-symmetry.org
sed -i 's/#+begin_src mermaid :file \([^ ]*\) :tangle \([^ ]*\)$/#+begin_src mermaid :file \1 :tangle \2 :mkdirp yes/g' examples/modular-systems-and-symmetry.org

# Double-check we didn't add duplicate :mkdirp yes
sed -i 's/:mkdirp yes :mkdirp yes/:mkdirp yes/g' examples/*.org

echo "Done adding :mkdirp yes to all tangle directives"