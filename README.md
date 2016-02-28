# ocp-ppx-debug
Small PPX to add prints at beginning and end of functions and match cases.

This ppx is at a really early development state. The idea is to be able to follow a really simple
state of the execution by adding prints at the beginning and end of each function and match case. For now, it 
needs an environment variable "PPX_DEBUG" to work, and set to something else than the empty string. It might
be improved later to be much more useful (without needing environment variable, adding a companion library
that helps controling the informations printed, adding some form of indentation).
