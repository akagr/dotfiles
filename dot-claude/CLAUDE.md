# Git commits
- **IMPORTANT** Never attribute claude in git commit messages
- **IMPORTANT** If the branch name contains a ticket id, include that in git commit message

# Pull Requests
- **IMPORTANT** Always try to use `gh` cli tool to interact with github, including for pull requests
- **IMPORTANT** Never attribute claude anywhere in pull requests
- **IMPORTANT** After creating a pull request, always open it in browser for review

# Post complete TODOS
- After completing todos, offer to commit and push the branch

# Tool Use
- **IMPORTANT** Whenever searching for documentation, automatically use context7 MCP server if available
- **IMPORTANT** Automatically use sequentialThinking mcp for planning. Try to use this tool for every plan.

# Planning
- **IMPORTANT** Ask upto 10 questions as needed to plan more accurately and eliminate as many unknowns as possible
- **IMPORTANT** Automatically use upto 10 tasks for parallel execution of plan and execution steps

# GitHub Actions: GITHUB_TOKEN Permissions
- To call `gh api repos/{owner}/{repo}/environments`, the job needs `actions: read` permission
- `environments` is NOT a valid permission key (will cause workflow parse error)
- `deployments: read` alone is NOT sufficient for the environments API
