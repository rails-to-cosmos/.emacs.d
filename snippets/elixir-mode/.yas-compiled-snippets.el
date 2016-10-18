;;; Compiled snippets and support files for `elixir-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'elixir-mode
                     '(("pry" "require IEx; IEx.pry\n" "pry" nil
                        ("debug")
                        nil "/Users/akatovda/.emacs.d/snippets/elixir-mode/pry" nil nil)
                       ("mdoc" "@moduledoc \"\"\"\n$0\n\"\"\"\n" "moduledoc" nil nil nil "/Users/akatovda/.emacs.d/snippets/elixir-mode/mdoc" nil nil)
                       ("iop" "IO.puts(\"$1 #{inspect $1}\")$0\n" "iop" nil nil nil "/Users/akatovda/.emacs.d/snippets/elixir-mode/iop" nil nil)
                       ("io" "IO.puts(\"$1\")$0\n" "io" nil nil nil "/Users/akatovda/.emacs.d/snippets/elixir-mode/io" nil nil)
                       ("hinfo" "def handle_info($1, state) do\n  $0\n  {:noreply, state}\nend\n" "hinfo" nil nil nil "/Users/akatovda/.emacs.d/snippets/elixir-mode/hinfo" nil nil)
                       ("hcast" "def handle_cast($1, state) do\n  $0\n  {:noreply, state}\nend\n" "hcast" nil nil nil "/Users/akatovda/.emacs.d/snippets/elixir-mode/hcast" nil nil)
                       ("hcall" "def handle_call($1, _from, state) do\n  reply = $0\n  {:reply, reply, state}\nend\n" "hcall" nil nil nil "/Users/akatovda/.emacs.d/snippets/elixir-mode/hcall" nil nil)
                       ("doc" "@doc \"\"\"\n$0\n\"\"\"\n" "doc" nil nil nil "/Users/akatovda/.emacs.d/snippets/elixir-mode/doc" nil nil)
                       ("do" "do\n  $0\nend\n" "do" nil nil nil "/Users/akatovda/.emacs.d/snippets/elixir-mode/do" nil nil)
                       ("defp" "defp $1 do\n  $0\nend\n" "defp" nil nil nil "/Users/akatovda/.emacs.d/snippets/elixir-mode/defp" nil nil)
                       ("defmodule" "defmodule $1 do\n  $0\nend\n" "defmodule" nil nil nil "/Users/akatovda/.emacs.d/snippets/elixir-mode/defmodule" nil nil)
                       ("defmacrop" "defmacrop $1 do\n  $0\nend\n" "defmacrop" nil nil nil "/Users/akatovda/.emacs.d/snippets/elixir-mode/defmacrop" nil nil)
                       ("defmacro" "defmacro $1 do\n  $0\nend\n" "defmacro" nil nil nil "/Users/akatovda/.emacs.d/snippets/elixir-mode/defmacro" nil nil)
                       ("def" "def ${1:method}${2:(${3:args})} do\n  $0\nend\n" "def" nil nil nil "/Users/akatovda/.emacs.d/snippets/elixir-mode/def" nil nil)
                       ("cond" "cond do\n  $0\nend\n" "cond" nil nil nil "/Users/akatovda/.emacs.d/snippets/elixir-mode/cond" nil nil)
                       ("cast" "GenServer.cast(${1:__MODULE__}, $0)\n" "cast" nil nil nil "/Users/akatovda/.emacs.d/snippets/elixir-mode/cast" nil nil)
                       ("case" "case $1 do\n  $0\nend\n" "case" nil nil nil "/Users/akatovda/.emacs.d/snippets/elixir-mode/case" nil nil)
                       ("call" "GenServer.call(${1:__MODULE__}, $0)\n" "call" nil nil nil "/Users/akatovda/.emacs.d/snippets/elixir-mode/call" nil nil)))


;;; Do not edit! File generated at Tue Oct 18 15:56:36 2016
