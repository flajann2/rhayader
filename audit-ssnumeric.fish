#!/usr/bin/env fish
# audit-ssnumeric.fish
#
# Finds every usage of SSNumeric/SSFloat/SSDouble in your Haskell source,
# shows it with context, and backs up touched files — so you can convert
# each site by hand (SSNumeric -> `a` + RealFloat constraint) with a safety net.
#
# This deliberately does NOT auto-rewrite: `SSNumeric` is a concrete type,
# `RealFloat` is a constraint, so the fix is structural (add `RealFloat a =>`
# to the enclosing signature, change the field/arg to `a`), not textual.

set -l dir (test -n "$argv[1]"; and echo $argv[1]; or echo .)
set -l backup_dir ".ssnumeric-backup-"(date +%Y%m%d-%H%M%S)

if not type -q rg
    echo "ripgrep (rg) not found — install it first: pacman/apt/brew install ripgrep"
    exit 1
end

echo "Scanning $dir for SSNumeric / SSFloat / SSDouble ..."
echo ""

set -l files (rg -l 'SSNumeric|SSFloat|SSDouble' $dir --type hs)

if test (count $files) -eq 0
    echo "No matches found under $dir."
    exit 0
end

echo "Found matches in "(count $files)" file(s):"
for f in $files
    echo "  $f"
end
echo ""

read -l -P "Back these files up before you start editing? [Y/n] " confirm
if test -z "$confirm" -o "$confirm" = "Y" -o "$confirm" = "y"
    mkdir -p $backup_dir
    for f in $files
        set -l dest "$backup_dir/$f"
        mkdir -p (dirname $dest)
        cp $f $dest
    end
    echo "Backed up to $backup_dir/"
end

echo ""
echo "--- Usage sites (grouped by file) ---"
echo ""

for f in $files
    echo "=== $f ==="
    rg -n --context 1 'SSNumeric|SSFloat|SSDouble' $f
    echo ""
end

echo "--- Summary ---"
rg -c 'SSNumeric' $dir --type hs 2>/dev/null | while read -l line
    echo "  $line  (SSNumeric occurrences)"
end

echo ""
echo "Next steps for each site above:"
echo "  1. Field/arg of type SSNumeric  -> change to a bare type variable 'a'"
echo "  2. Enclosing signature/data decl -> add 'RealFloat a =>' (or your RealNum alias)"
echo "  3. Constructors SSFloat x / SSDouble x -> just 'x' (no wrapper needed)"
echo "  4. Case matches on SSFloat/SSDouble -> delete, operate on 'a' directly"
echo "  5. Once all sites compile, delete the SSNumeric module and its imports"
