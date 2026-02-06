# 📊 Validation Dashboard Setup

This guide will help you set up the GitHub Pages dashboard for displaying metadata validation results.

## ✅ What's Been Created

The following files have been created for your GitHub Pages dashboard:

```
docs/
├── README.md                    # Detailed documentation
├── index.html                   # Main dashboard page
├── validation_results.json      # Sample data (will be auto-generated)
└── assets/
    ├── style.css               # Dashboard styling
    └── validation.js           # Dashboard functionality
```

The GitHub Actions workflow has also been updated to:
- Generate validation results in JSON format
- Automatically deploy to GitHub Pages

## 🚀 Quick Setup (3 Steps)

### Step 1: Enable GitHub Pages

1. Go to your repository on GitHub: `https://github.com/waldronlab/curatedMetagenomicDataCuration`
2. Click **Settings** → **Pages**
3. Under **Source**, select:
   - **Branch**: `gh-pages` (will be created automatically)
   - **Folder**: `/ (root)`
4. Click **Save**

### Step 2: Set Workflow Permissions

1. In your repository, go to **Settings** → **Actions** → **General**
2. Scroll down to **Workflow permissions**
3. Select **Read and write permissions**
4. Check **Allow GitHub Actions to create and approve pull requests**
5. Click **Save**

### Step 3: Trigger the Workflow

Run the workflow to generate the first deployment:

**Option A: Push your changes**
```bash
git add docs/ .github/workflows/validate-metadata.yml
git commit -m "Add GitHub Pages validation dashboard"
git push origin master
```

**Option B: Manual trigger**
1. Go to **Actions** tab
2. Click "Validate Metadata Against Latest Schema"
3. Click **Run workflow** → **Run workflow**

## 🌐 Accessing Your Dashboard

After the workflow completes (2-5 minutes), your dashboard will be available at:

```
https://waldronlab.github.io/curatedMetagenomicDataCuration/
```

## 📸 Dashboard Features

Your dashboard will show:

- ✅ **Overall validation status** (Pass/Fail)
- 📁 **Total files validated**
- ❌ **Number of errors**
- ⚠️ **Number of warnings**
- 📋 **Detailed list of studies with issues**
- ⏰ **Last update timestamp**
- 🔗 **Link to workflow run**
- 📊 **Schema version information**

## 🔄 Automatic Updates

The dashboard updates automatically when:

1. **Code is pushed** to master with metadata changes
2. **Pull requests** are created (validation only, no deployment)
3. **Daily at 2 AM UTC** (scheduled check)
4. **Schema updates** in OmicsMLRepoCuration repository
5. **Manual trigger** from Actions tab

## 🧪 Testing Locally

To preview the dashboard locally before deployment:

1. Open `docs/index.html` in your web browser
2. You'll see the sample data (all passing)
3. Once the workflow runs, real data will replace the sample

## 📖 Detailed Documentation

For more information, see:
- [docs/README.md](docs/README.md) - Complete dashboard documentation
- [.github/workflows/validate-metadata.yml](.github/workflows/validate-metadata.yml) - Workflow configuration

## 🐛 Troubleshooting

### "404 - Page not found"
- Wait 5-10 minutes after first deployment
- Check that `gh-pages` branch was created
- Verify GitHub Pages is enabled in Settings

### Dashboard shows "Unable to Load Validation Results"
- Ensure the workflow has run at least once
- Check the Actions tab for errors
- Verify `validation_results.json` exists in `gh-pages` branch

### Changes not appearing
- Hard refresh: `Ctrl+Shift+R` (Windows/Linux) or `Cmd+Shift+R` (Mac)
- Check the Actions tab to confirm deployment completed
- Browser cache may need clearing

## 🎉 Next Steps

Once your dashboard is live:

1. Share the URL with your team
2. Bookmark it for quick access
3. Use it to monitor validation status
4. Check it before merging pull requests

## 💡 Need Help?

- Check [docs/README.md](docs/README.md) for detailed troubleshooting
- Review the Actions logs for error messages
- Ensure all required secrets and permissions are configured

---

**Happy validating! 🔬**
