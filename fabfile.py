from fabric.api import local

def push():
    local('git add -p && git commit -am\'Fabric automatic commit\'')
    local('git push')
