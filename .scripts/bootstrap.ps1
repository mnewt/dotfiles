# Bootstrap the a repo on a new Windows computer

Param(
[Parameter(Position = 0)][string]$Remote,
[Parameter(Position = 1)][string]$Local = "$HOME"
)

$RepoDir = "$HOME\.config\repos"
$Repo = "$RepoDir\$([io.path]::GetFileNameWithoutExtension($Remote))"
$RepoTemp = "$RepoDir\temp"

Push-Location "$HOME"

if (Test-Path "$Repo") {
if ((Read-Host "Delete the existing directory at $($Repo)? [y/N]").ToLower() -ne 'y') {
Write-Host "Not modifying anything."
Exit
}
else {
Write-Host "Deleting directory $Repo..."
Remove-Item -Force -Recurse "$Repo"
}
}

if (Test-Path "$HOME/.git") {
if ((Read-Host "Delete $($HOME)\.git? [y/N]").ToLower() -ne 'y') {
Write-Host "Not modifying anything."
Exit
}
else {
Write-Host "Deleting $($HOME)\.git..."
Remove-Item -Force -Recurse "$HOME\.git"
}
}

git.exe clone --no-checkout --separate-git-dir "$Repo" "$Remote" "$RepoTemp"
Set-Content -Path "$HOME\.git" -Value "gitdir: $($Repo)"
git.exe config --local status.showUntrackedFiles no
git.exe fetch

Write-Host "Listing conflicting files..."
git.exe diff --name-status
if ((Read-Host "Merge into $HOME, overwriting the conflicting files? [y/N]") -eq 'y') {
Write-Host "Overwriting now..."
git.exe reset --merge HEAD
}
else {
Write-Host "Not overwriting $HOME."
Exit
}

New-Item 'HKCU:\Software\GNU\Emacs' -Force | New-ItemProperty -Name HOME -Value "$HOME" -Force

Pop-Location

