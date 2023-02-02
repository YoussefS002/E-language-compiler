#!/usr/bin/python3
"""Test orchestration for ecomp"""
import argparse
import glob
from threading import Thread
import json
from subprocess import Popen, PIPE
import signal
import sys
import time
import textwrap
import difflib
from typing import List
import datetime

# The displayer thread shows what commands are still running
class Displayer(Thread):
    """A Displayer thread"""

    def __init__(self, threads):
        Thread.__init__(self)
        self.threads = threads

    def run(self):
        width = 120
        l_running = [x for x in self.threads if x.running]
        while len(l_running) > 0:
            s_running = "{} threads running ".format(len(l_running))
            thrs = str(list(map(lambda x: x.filename,
                                l_running)))[:(width - len(s_running))]
            print("\r{}({}).{}".format(
                s_running, thrs, " " * (width - len(thrs) - len(s_running))),
                  end="")
            time.sleep(1)
            l_running = [x for x in self.threads if x.running]
        print()




def display_verbatim(body):
    "Show verbatim contents"
    return '\n'.join([
        '\n'.join(
            textwrap.wrap(line,
                          90,
                          break_long_words=False,
                          replace_whitespace=False))
        for line in body.splitlines() if line.strip() != ''
    ])


def make_td(td):
    "Create a TD HTML tag"
    if isinstance(td, list):
        text = td[0]
        opts = " ".join(td[1:])
    else:
        text = str(td)
        opts = ""

    return "<td {}>{}</td>".format(opts, text)

icon_ok = "<i class='fa fa-check' style='color: green;'></i>"
icon_warn = "<i class='fa fa-exclamation-circle' style='color: orange;'></i>"
icon_ko = "<i class='fa fa-times-circle' style='color:red;'></i>"

# CommandExecutor run a command [cmd] and operates on file [f]
class CommandExecutor(Thread):
    """Command Executor"""
    def __init__(self, filename, cmd, args, make_expect, numcols):
        Thread.__init__(self)
        self.filename = filename
        self.cmd = cmd
        self.s = ""
        self.running = True
        self.stdout = ""
        self.stderr = ""
        self.lastcorrectstep = -1
        self.proc = None
        self.args = args
        self.make_expect = make_expect
        self.numcols = numcols

    # run cmd and get stdout and stderr in dict
    def run_capture_output_interruptible(self, cmd):
        self.proc = Popen(cmd, stdout=PIPE, stderr=PIPE)
        try:
            self.stdout, self.stderr = self.proc.communicate()
            self.proc.poll()
            self.stdout = self.stdout.decode('utf8')
            self.stderr = self.stderr.decode('utf8')
        except:
            self.stdout = (b"[]").decode('utf8')
            self.stderr = "".decode('utf8')

    def stop(self):
        """Stop the thread"""
        if self.proc:
            self.proc.kill()

    def get_test_results(self, json_file_name):
        "Get test results, handle errors"
        try:
            with open(json_file_name, 'r') as jsonfile:
                try:
                    return json.load(jsonfile)
                except:
                    return [{
                        'retval': -1,
                        'output': display_verbatim(self.stdout),
                        'error': display_verbatim(self.stderr)
                    }]
        except:
            return [{
                'retval': -1,
                'output': display_verbatim(self.stdout),
                'error': "No file {} generated...".format(json_file_name)
            }]

    @staticmethod
    def register_expect(expect_file_name, out, err, ret):
        "Creates a .expect file"
        with open(expect_file_name, 'w') as expectfile:
            json.dump(
                {
                    'output': out,
                    'error': err,
                    'retval': ret
                }, expectfile)

    @staticmethod
    def check_expect(expect_file_name, out, err, ret):
        "Checks a result wrt an expect file"
        try:
            with open(expect_file_name, 'r') as expectfile:
                j = json.load(expectfile)
                return j['retval'] == ret and \
                    j['output'] == out and \
                    j['error'] == err
        except:
            return None
     

    def run(self):
        """Actually run the test"""
        self.run_capture_output_interruptible(self.cmd.split(" "))
        json_file_name = self.filename + ".json"
        j = self.get_test_results(json_file_name)
        self.s = ""
        curcol = 0
        first_runstep = True
        for _, r in enumerate(j, start=0):
            if "runstep" in r:
                expect_file_name = self.filename + ".expect_" + "_".join(self.args)
                if first_runstep and self.make_expect:
                    self.register_expect(expect_file_name, r['output'], r['error'], r['retval'])
                first_runstep = False
                cls = "good"
                test_ok = self.check_expect(expect_file_name, r['output'], r['error'], r['retval'])
                if test_ok is None:
                    cls = ""
                else:
                    cls = "good" if test_ok else "bad"
                if cls == "good":
                    self.lastcorrectstep = curcol
                err_str = "Error: <pre>" + r['error'] + "</pre>" if r['error'] is not None else ""
                self.s += make_td([
                    "{}<div name='{}' style='display:none;'>Ret = {}.<br>Output = <pre>'{}'</pre>{}<br>Time: {:.2f} seconds.</div>"
                    .format(
                        icon_ok if cls == 'good' else icon_ko,
                        self.filename,
                        r['retval'],
                        r['output'],
                        err_str,
                        r['time']),
                    "class=\"{}\"".format(cls)
                ]) + "\n"
                curcol += 1
            elif "compstep" in r:

                compstep_td = ""
                err = r['error']
                if err is not None:
                    compstep_td = """
                    <td class="bad" style="text-align: left;" colspan="{}">{}<div name='{}' style='display:none'>{} error:<br><pre>{}</pre></div></td>
                    """.format(self.numcols - curcol,
                               icon_ko,
                               self.filename,
                               r['compstep'], err)

                elif r["compstep"] == "Lexing":
                    expect_lex_file_name = self.filename + ".expect_lexer"
                    out_lex_file_name = self.filename[:-2] + ".lex"
                    try:
                        with open(expect_lex_file_name, "r") as expect_lex_file, \
                             open(out_lex_file_name, "r") as out_lex_file:
                            expected_tokens = expect_lex_file.readlines()
                            out_tokens = out_lex_file.readlines()
                            diff = difflib.unified_diff(
                                expected_tokens,
                                out_tokens,
                                fromfile=expect_lex_file_name,
                                tofile=out_lex_file_name)
                            diff = list(diff)
                            if diff == []:
                                compstep_td = "<td class=\"good\">{}</td>".format(icon_ok)
                            else:
                                compstep_td = "<td class=\"warn\" style=\"text-align: left;\" >{}<div name='{}'>Lexing not-what-expected:<br><pre>{}</pre></div></td>".format(icon_warn, self.filename, "".join(diff))
                    except:
                        compstep_td = "<td>No .expect_lexer file</td>"

                self.s += compstep_td + "\n"
            else:
                err = r['error']
                if err is not None:
                    self.s += """
                    <td class="bad" style="text-align: left;" colspan="{}">error:<br><pre>{}</pre></td>
                    """.format(self.numcols - curcol, err) + "\n"
                else:
                    self.s += """
                    <td class="bad" colspan="{}">error:<br><pre>{}</pre></td>
                    """.format(self.numcols - curcol, r) + "\n"

        self.s = """
<tr>
        <td class="rowname">
           <a href="{}.html">{}</a>
        <input class="w3-btn" id='toggle{}' type="button" onclick="toggle('{}')" value="+"/>
        </td>
        {}
</tr>""".format(self.filename, self.filename,
                self.filename, self.filename, self.s) + "\n"

        self.running = False


def interrupt_handler(threads):
    "Interrupt handler"
    def int_handler(_sig, _frame):
        "Interrupt handler"
        print('You pressed Ctrl+C!')
        for thr in threads:
            thr.stop()
    return int_handler

def get_args():
    "Get arguments"
    parser = argparse.ArgumentParser()
    parser.add_argument("-f",
                        "--file",
                        help="files to compile",
                        default=glob.glob("*.e"),
                        nargs='+')
    parser.add_argument("-p",
                        "--passes",
                        help="passes to execute",
                        nargs='+',
                        default=[
                            "e-run", "cfg-run", "cfg-run-after-cp",
                            "cfg-run-after-dae", "cfg-run-after-ne", "rtl-run",
                            "linear-run", "linear-run-after-dse", "ltl-run",
                            "riscv-run"
                        ])
    parser.add_argument("--args",
                        help="args for programs",
                        nargs='+',
                        default=["14", "12", "3", "8", "12"])
    parser.add_argument("--html", help="Output HTML file", default="results.html")
    parser.add_argument("-d",
                        "--dry-run",
                        help="Dry-run. Don't actually compile anything",
                        action="store_true")
    parser.add_argument("-v",
                        "--verbose",
                        help="Verbosity level",
                        action="count",
                        default=0)
    parser.add_argument("--make-expect",
                        help="Make .expect files for each test",
                        action="store_true")
    args, unknown_args = parser.parse_known_args()
    return args, unknown_args

def main():
    "Main function"
    args, unknown_args = get_args()
    # show options
    if args.verbose >= 1:
        print("args.file=" + str(args.file))
        print("args.passes=" + str(args.passes))
        print("args.args=" + str(args.args))
        print("args.html=" + str(args.html))
        print("args.dry_run=" + str(args.dry_run))
        print("args.verbose=" + str(args.verbose))

    # construct the set of commands to be launched, one per file
    cmds = []
    for fname in args.file:
        cmd = "../ecomp -json {f}.json -f {f} {passes} {uargs} -- {args}"
        cmd = cmd.format(passes=" ".join(map(lambda s: "-" + s, args.passes)),
                         uargs=" ".join(unknown_args),
                         args=" ".join(args.args),
                         f=fname)
        cmds.append((fname, cmd))

    # dyr_run : simply show the commands but don't execute them
    if args.dry_run:
        for (_, cmd) in cmds:
            print(cmd)
        sys.exit()

    # The list of threads that will be launched
    threads: List[Thread] = []



    signal.signal(signal.SIGINT, interrupt_handler(threads))

    for (fname, cmd) in cmds:
        exec_thread = CommandExecutor(fname, cmd,
                                      args.args, args.make_expect,
                                      # 1 colonne pour le lexer
                                      len(args.passes) + 1)
        threads.append(exec_thread)
        exec_thread.start()

    print("Before launching displayer: {} threads".format(len(threads)))

    disp_thread = Displayer(list(threads))
    disp_thread.start()

    for thr in threads:
        thr.join()

    disp_thread.join()
    print("After displayer: {} threads".format(len(threads)))
    print("All threads terminated!")

    res_html = open(args.html, "w")
    res_html.write("""
    <html>
    <head>
    <link rel="stylesheet" href="w3.css">
    <script src="https://kit.fontawesome.com/1f5d81749b.js" crossorigin="anonymous"></script>
    <style type="text/css">
    /*
    table , table th, table td , table tr{
        border: 1px solid black;
        border-collapse: collapse;
        text-align: center;
    }
    .rowname, th {
        background-color: #ccc;
    }
    td {
        padding: 0.4em;
    }
    .bad{
        background-color: #f9d7dc;
    }
    .good{
        background-color: #c7f0d2;
    }
    .warn{
        background-color: orange;
    }
    
    fieldset {
        display: inline;
        margin: 1em;
        padding: 1em;
    }*/
    </style>
    <script type="text/javascript">
    function toggleVisibility(elt){
      if (elt.style.display == 'none')
        elt.style.display = 'block';
      else
        elt.style.display = 'none';
    }

    function toggle(name){
      var elts = document.getElementsByName(name);
      console.log(elts);
      for (var k = 0; k < elts.length; k++){
        toggleVisibility(elts[k]);
      }
      var x = document.getElementById('toggle'+name);
      if (x.value == '+') x.value = '-';
      else x.value = '+';
    }
    </script>
    </head>
    <body>""")

    now = datetime.datetime.now()
    res_html.write ("{}".format(now))

    res_html.write("""
    <table class="w3-table w3-striped w3-responsive">
    <tr><th>File</th>""")
    for pass_name in ["Lexer"] + args.passes:
        res_html.write("<th style='transform: rotate(180deg); writing-mode: vertical-rl;'>{}</th>\n".format(pass_name))
    res_html.write("""
    </tr>
    """)

    print("Before sorting: {} threads".format(len(threads)))
    threads = sorted(threads, key=lambda t: (t.lastcorrectstep, t.filename), reverse=False)
    print("After sorting: {} threads".format(len(threads)))
    for thr in threads:
        res_html.write(thr.s)
        res_html.write("\n")
    res_html.write("</table>\n")
    res_html.write("</body>\n")
    res_html.write("</html>\n")
    res_html.close()

    numtotal = len(threads)
    thr_ok = [t for t in threads if t.lastcorrectstep == len(args.passes) - 1]
    thr_ko = [t for t in threads if t.lastcorrectstep != len(args.passes) - 1]

    print("{}/{} OK.".format(len(thr_ok), numtotal))
    print("{}/{} KO : {}".format(
        len(thr_ko), numtotal, list(map((lambda t: (t.filename, t.lastcorrectstep)), thr_ko))))

    if args.verbose >= 1:
        for thr in thr_ko + (thr_ok if args.verbose >= 2 else []):
            print(thr.filename)
            print("STDOUT: \n{}".format(thr.stdout))
            print("STDERR: \n{}".format(thr.stderr))

if __name__ == "__main__":
    main()
