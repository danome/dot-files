# API Key Configuration

## IMPORTANT SECURITY NOTE

**NEVER** commit API keys or other secrets directly in configuration files. Always use environment variables.

## Setting up OpenAI API Key

The Emacs configuration now expects the OpenAI API key to be set as an environment variable.

### Option 1: Set in .bashrc.local (Recommended)

Create or edit `~/.bashrc.local` (which is not tracked in git) and add:

```bash
export OPENAI_API_KEY="your-actual-api-key-here"
```

### Option 2: Set in .profile or .bash_profile

Add to your `~/.profile` or `~/.bash_profile`:

```bash
export OPENAI_API_KEY="your-actual-api-key-here"
```

### Option 3: Use a secure secrets manager

Consider using a secrets management tool like:
- `pass` (the standard unix password manager)
- `keepassxc` with CLI integration
- Cloud provider secret managers (AWS Secrets Manager, etc.)

## Verifying the setup

After setting the environment variable and restarting your shell, verify with:

```bash
echo $OPENAI_API_KEY | head -c 10
```

This should show the first 10 characters of your API key.

## URGENT: Revoke Compromised Keys

If you previously had API keys hardcoded in your configuration files that were pushed to a public repository, you should:

1. **Immediately revoke those keys** through the OpenAI dashboard
2. Generate new API keys
3. Update your environment variables with the new keys
4. Never commit the new keys to version control

## Related Emacs Configuration

The following Emacs packages now use the `OPENAI_API_KEY` environment variable:
- `gpt` package
- `org-ai` package  
- `gptel` package

They retrieve the key using: `(getenv "OPENAI_API_KEY")`
