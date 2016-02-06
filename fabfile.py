from fabric.api import local, settings


def push():
    with settings(prompts={'Stage this hunk [y,n,q,a,d,/,s,e,?]? ': 'y'}):
        local('git add -p && git commit -am\'Fabric automatic commit\'')
        local('git push')
