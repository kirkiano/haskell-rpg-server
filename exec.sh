# tcpdump 'tcp[tcpflags] & (tcp-syn|tcp-fin|tcp-rst) != 0' >> tcp.log 2>&1 &

stack exec haskell-rpg-server-exe -- "$@"
