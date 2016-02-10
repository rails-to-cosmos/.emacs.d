from fabric.api import local, settings


def push():
    with settings():
        local('git add -p && git commit -m\'Fabric autocommit\'')
        local('git push')
