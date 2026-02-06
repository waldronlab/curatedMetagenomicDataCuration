# GitHub Pages Setup for Validation Dashboard

This directory contains the GitHub Pages site that displays metadata validation results.

## 🌐 Live Dashboard

Once configured, the validation dashboard will be available at:
```
https://waldronlab.github.io/curatedMetagenomicDataCuration/
```

## 📋 Setup Instructions

### 1. Enable GitHub Pages

1. Go to your repository on GitHub
2. Navigate to **Settings** → **Pages**
3. Under **Source**, select:
   - **Branch**: `gh-pages`
   - **Folder**: `/ (root)`
4. Click **Save**

The site will be published automatically after the next workflow run.

### 2. Configure Workflow Permissions

The workflow needs write permissions to deploy to GitHub Pages:

1. Go to **Settings** → **Actions** → **General**
2. Scroll to **Workflow permissions**
3. Select **Read and write permissions**
4. Check **Allow GitHub Actions to create and approve pull requests**
5. Click **Save**

### 3. Verify Deployment

After the workflow runs:

1. Go to **Actions** tab
2. Look for the "Validate Metadata Against Latest Schema" workflow
3. Check that the "Deploy to GitHub Pages" step completed successfully
4. Visit your GitHub Pages URL to view the dashboard

## 🔄 How It Works

### Workflow Process

1. **Validation**: The GitHub Actions workflow validates all metadata files against the latest schema
2. **JSON Generation**: Validation results are compiled into `validation_results.json`
3. **Deployment**: The entire `docs` folder is deployed to the `gh-pages` branch
4. **Display**: The dashboard loads and displays the JSON data

### Files in this Directory

- **`index.html`**: Main dashboard page with responsive layout
- **`assets/style.css`**: Styling for the dashboard
- **`assets/validation.js`**: JavaScript to load and display validation data
- **`validation_results.json`**: Generated JSON file with validation results (created by workflow)

## 🎨 Features

### Dashboard Components

- **Summary Cards**: Overview of validation status, file count, errors, and warnings
- **Metadata Section**: Information about the validation run (timestamp, trigger, branch, schema version)
- **Issues Display**: Detailed list of studies with errors or warnings
- **Success Message**: Displayed when all validations pass
- **Responsive Design**: Works on desktop, tablet, and mobile devices

### Visual Indicators

- 🟢 **Green badges**: Validation passed
- 🔴 **Red badges**: Validation failed
- ⚠️ **Yellow badges**: Warnings present
- **Color-coded cards**: Errors (red), warnings (yellow)

## 🔧 Customization

### Updating Styles

Edit [`assets/style.css`](assets/style.css) to modify colors, fonts, or layout.

### Modifying Dashboard Layout

Edit [`index.html`](index.html) to change the structure or add new sections.

### Extending Functionality

Edit [`assets/validation.js`](assets/validation.js) to add features like:
- Historical data tracking
- Filtering and search
- Export functionality
- Additional visualizations

## 🚀 Triggering Updates

The dashboard updates automatically when:

1. **Push to master**: Any changes to `inst/harmonized/**/*.tsv` files
2. **Pull requests**: Validation runs but doesn't deploy (only on master)
3. **Daily schedule**: Runs at 2 AM UTC
4. **Manual trigger**: Via Actions tab → "Run workflow"
5. **Schema updates**: When OmicsMLRepoCuration schema changes

## 📊 JSON Data Format

The `validation_results.json` file has the following structure:

```json
{
  "status": "PASS" | "FAIL",
  "summary": {
    "total_files": 123,
    "total_errors": 0,
    "total_warnings": 5,
    "files_with_issues": 2
  },
  "metadata": {
    "timestamp": "2024-01-15T14:30:00Z",
    "trigger": "push",
    "branch": "master",
    "schema_commit": "abc123...",
    "schema_commit_short": "abc123",
    "run_id": "123456789"
  },
  "studies_with_issues": [
    {
      "name": "StudyName_2021",
      "file": "inst/harmonized/StudyName_2021/StudyName_2021_sample.tsv",
      "rows": 100,
      "cols": 50,
      "errors": ["Error message 1", "Error message 2"],
      "warnings": ["Warning message 1"]
    }
  ]
}
```

## 🐛 Troubleshooting

### Dashboard Not Loading

1. Check that GitHub Pages is enabled in repository settings
2. Verify the `gh-pages` branch exists and contains the `docs` folder
3. Check the Actions tab for deployment errors
4. Wait 5-10 minutes after first deployment for DNS propagation

### Validation Results Not Updating

1. Verify the workflow completed successfully in the Actions tab
2. Check that the "Deploy to GitHub Pages" step ran
3. Hard refresh the page (Ctrl+Shift+R or Cmd+Shift+R)
4. Check browser console for JavaScript errors

### JSON File Not Found Error

1. Ensure at least one workflow run has completed successfully
2. Check that `docs/validation_results.json` exists in the `gh-pages` branch
3. Verify the R script in the workflow generates the JSON file

## 📖 Related Documentation

- [GitHub Pages Documentation](https://docs.github.com/en/pages)
- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [OmicsMLRepoCuration Schema](https://github.com/shbrief/OmicsMLRepoCuration)

## 🤝 Contributing

To improve the dashboard:

1. Make changes to files in the `docs` directory
2. Test locally by opening `index.html` in a browser
3. Commit and push changes
4. The dashboard will update automatically on the next workflow run

---

**Note**: This dashboard is automatically generated by GitHub Actions. Manual edits to `validation_results.json` will be overwritten on the next workflow run.
