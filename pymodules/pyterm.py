# [[file:~/Documents/Stuff/ya.org::run_terminal][run_terminal]]
script="ls -la"
script = script.split('\n\t')
cmds = [cmd.strip() for cmd in script if cmd.strip()]
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
# run_terminal ends here
