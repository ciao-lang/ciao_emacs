:- bundle(ciao_emacs).
% Manifest file for Ciao emacs mode
version('1.20.0').
%
depends([core
     % lpdoc % (optional)
     ]).
%alias_paths([ciao_emacs = '.']).
cmd('ciao-emacs', [main='cmds/ciao_emacs']).


