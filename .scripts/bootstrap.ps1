# Bootstrap the dotfiles repo on a new Windows computer

$repo_remote = "https://gitlab.com/mnewt/dotfiles.git"
$repo_dir = "$HOME/.config/repos"
$repo_temp = "$HOME/.config/repos/temp"

New-Item -ItemType Directory "$repo_dir"
if (Test-Path "$HOME/.git") { Remove-Item -Force -Recurse "$HOME/.git" }
git clone --separate-git-dir "$repo_dir/dotfiles" "$repo_remote" --no-checkout "$repo_temp"
Move-Item "$repo_temp/.git" "$HOME"
Remove-Item -Force -Recurse "$repo_temp"
git fetch
git reset --hard HEAD
git config --local status.showUntrackedFiles no
