$OutputEncoding = [console]::InputEncoding = [console]::OutputEncoding = [System.Text.UTF8Encoding]::new()

Write-Output "Deploying updates to GitHub..."

# Build the project.
hugo

# Add changes to git.
git add .

# Commit changes.
$msg = "Rebuilding site $(date)"

if ($Args.Count -eq 1) {
    $msg = $Args[0]
}

git commit -m "$msg"

# Push source and build repos.
git push origin source
git subtree push --prefix docs/ origin master