## Makefile

# This program is free software: you can redistribute it and/or modify
# it under the terms of the Affero GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the Affero GNU General Public License for more details.

# You should have received a copy of the Affero GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

EMACS ?= emacs
EFLAGS ?= -Q -L . -L ./test

CASK ?= cask

ELS = navbar.el navbar-items.el
ELCS = $(ELS:.el=.elc)

.PHONY: compile
compile: test/elscreen.elc $(ELCS)

%.elc: %.el
	$(EMACS) $(EFLAGS) --batch -f batch-byte-compile $<

.PHONY: clean
clean:
	rm -f $(ELCS)
	rm -rf dist

.PHONY: distclean
distclean: clean
	rm -f test/elscreen.el
	rm -rf .cask

.PHONY: test
test: compile
	$(EMACS) $(EFLAGS) --batch -l test/navbar-test.el -f ert-run-tests-batch-and-exit

.PHONY: test-interactive
test-interactive: compile
	$(EMACS) $(EFLAGS) -nw -l test/navbar-test.el --eval "(ert t)"
	$(EMACS) $(EFLAGS) -l test/navbar-test.el --eval "(ert t)"

.PHONY: test-all
test-all: test test-interactive

test/elscreen.el:
	curl -sSL https://github.com/emacs-jp/elscreen/raw/master/elscreen.el > $@

.PHONY: package
package:
	$(CASK) package

.PHONY: test-package
test-package: package
	$(CASK) exec $(EMACS) --batch -f package-initialize --eval '(package-install-file (car (file-expand-wildcards "$(shell pwd)/dist/navbar-*.tar")))'
	$(CASK) exec $(EMACS) --batch -f package-initialize -l test/navbar-test.el -f ert-run-tests-batch-and-exit
	$(CASK) exec $(EMACS) --batch -f package-initialize -l undercover --eval '(undercover "navbar.el")' -l navbar.el -l test/navbar-test.el -f ert-run-tests-batch-and-exit
