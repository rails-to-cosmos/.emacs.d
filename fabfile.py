from fabric.api import local, settings


def push(cm):
    with settings():
        local('git commit -am\'{cm}\''.format(cm=cm))
        local('git pull origin master')
        local('git push origin master')
