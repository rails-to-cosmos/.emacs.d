from fabric.api import local
from ilogue.fexpect import expect, expecting, run

prompts = [expect('Stage this hunk [y,n,q,a,d,/,e,?]?', 'y')]

def push():
    with expecting(prompts):
        local('git add -p && git commit -am\'Fabric automatic commit\'')
        local('git push')
