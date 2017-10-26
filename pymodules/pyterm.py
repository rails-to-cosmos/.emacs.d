import subprocess

def run_term(script):
    script = script.split('\n\t')
    cmds = '\n\t'.join(['do script "{cmd}" in front window'.format(cmd=cmd.strip()) for cmd in script if cmd.strip()])
    script = '''
    tell application "Terminal"
      activate
      {}
    end tell
    '''.format(cmds)
    proc = subprocess.Popen(
        ['osascript', '-'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE
    )
    return proc.communicate(script)[0]

if __name__ == '__main__':
    run_term('ls -la')
