#!/usr/bin/env bash

clone ()
{
    git clone https://github.com/$1.git ~/Clones/$2
}

if  command -v git &>/dev/null 
    then
        clone skywind3000/z.lua z.lua
fi


