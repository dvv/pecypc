PROJECT = pecypc

#DEPS = lager cowboy mimetypes erlydtl stable
DEPS = lager cowboy erlydtl stable

dep_lager = https://github.com/basho/lager.git HEAD
dep_cowboy = https://github.com/extend/cowboy.git HEAD
#dep_mimetypes = https://github.com/dvv/mimetypes.git HEAD
dep_erlydtl = https://github.com/evanmiller/erlydtl.git HEAD
dep_stable = https://github.com/dvv/stable.git HEAD

#%  {pmod_transform, "", {git, "https://github.com/erlang/pmod_transform.git", {branch, "HEAD"}}},
#  {hackney, "", {git, "https://github.com/benoitc/hackney.git", {branch, "HEAD"}}},
#  {gproc, "", {git, "https://github.com/uwiger/gproc.git", {branch, "HEAD"}}}

include erlang.mk

