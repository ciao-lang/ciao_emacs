:- bundle(ciao_emacs).
% Manifest file for Ciao emacs mode
version('1.21.0-alpha.4').
%
depends([core
     % lpdoc % (optional)
     ]).
%alias_paths([ciao_emacs = '.']).
cmd('ciao-emacs', [main='cmds/ciao_emacs']).


