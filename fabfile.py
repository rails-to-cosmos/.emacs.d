from fabric.api import local

def push():
    local('git commit -am\'Fabric automatic commit\'')
    local('git push')
