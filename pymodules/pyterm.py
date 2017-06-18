script="ls"
cmds = [cmd.strip() for cmd in script.split('\n\t') if cmd.strip()]
ascmds = '\n\t'.join(['do script "{cmd}" in front window'.format(cmd=cmd) for cmd in cmds])
script = '''
tell application "Terminal"
  activate
  {ascmds}
end tell
'''.format(ascmds=ascmds)

import subprocess
proc = subprocess.Popen(
    ['osascript', '-'],
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE
)
stdout_output = proc.communicate(script)[0]
print stdout_output, type(proc)
