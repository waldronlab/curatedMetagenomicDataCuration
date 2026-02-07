// Configuration
const VALIDATION_DATA_URL = 'validation_results.json';
const REPO_OWNER = 'waldronlab';
const REPO_NAME = 'curatedMetagenomicDataCuration';

// Load and display validation results
async function loadValidationResults() {
    try {
        const response = await fetch(VALIDATION_DATA_URL);
        if (!response.ok) {
            throw new Error(`Failed to fetch validation results: ${response.status}`);
        }
        
        const data = await response.json();
        displayResults(data);
        hideLoading();
    } catch (error) {
        console.error('Error loading validation results:', error);
        showError(error.message);
    }
}

function displayResults(data) {
    // Update summary cards
    document.getElementById('overall-status').textContent = data.status;
    document.getElementById('overall-status').className = `status-badge status-${data.status.toLowerCase()}`;
    
    document.getElementById('total-files').textContent = data.summary.total_files || 0;
    document.getElementById('total-errors').textContent = data.summary.total_errors || 0;
    document.getElementById('total-warnings').textContent = data.summary.total_warnings || 0;
    
    // Update status card color
    const statusCard = document.getElementById('status-card');
    if (data.status === 'PASS') {
        statusCard.classList.add('status-pass');
    } else {
        statusCard.classList.add('status-fail');
    }
    
    // Update metadata
    document.getElementById('last-updated').textContent = formatDate(data.metadata.timestamp);
    document.getElementById('trigger').textContent = data.metadata.trigger || 'N/A';
    document.getElementById('branch').textContent = data.metadata.branch || 'N/A';
    
    const schemaCommit = data.metadata.schema_commit_short || 'N/A';
    const schemaLink = schemaCommit !== 'N/A' 
        ? `<a href="https://github.com/shbrief/OmicsMLRepoCuration/commit/${data.metadata.schema_commit}" target="_blank">${schemaCommit}</a>`
        : schemaCommit;
    document.getElementById('schema-commit').innerHTML = schemaLink;
    
    // Update workflow link
    if (data.metadata.run_id) {
        const workflowUrl = `https://github.com/${REPO_OWNER}/${REPO_NAME}/actions/runs/${data.metadata.run_id}`;
        document.getElementById('workflow-link').href = workflowUrl;
    }
    
    // Display studies with issues
    if (data.studies_with_issues && data.studies_with_issues.length > 0) {
        displayStudiesWithIssues(data.studies_with_issues);
        document.getElementById('issues-section').style.display = 'block';
        document.getElementById('success-section').style.display = 'none';
    } else {
        document.getElementById('issues-section').style.display = 'none';
        document.getElementById('success-section').style.display = 'block';
    }

    // Display harmonized metadata stats
    if (data.stats) {
        displayStats(data.stats);
    }
    
    // Show content
    document.getElementById('content').style.display = 'block';
}

function displayStats(stats) {
    document.getElementById('total-studies').textContent = stats.total_studies ?? 0;
    document.getElementById('total-samples').textContent = stats.total_samples ?? 0;

    const distributions = stats.distributions || {};
    renderDistribution('species', distributions.species);
    renderDistribution('age-group', distributions.age_group);
    renderDistribution('body-site', distributions.body_site);
    renderDistribution('published-year', distributions.published_year);
    renderDistribution('country', distributions.country);
    renderDistribution('ancestry', distributions.ancestry);
    renderDistribution('disease', distributions.disease);
    renderDistribution('sex', distributions.sex);
}

function renderDistribution(prefix, distribution) {
    const metaEl = document.getElementById(`${prefix}-meta`);
    const listEl = document.getElementById(`${prefix}-list`);

    if (!distribution || !Array.isArray(distribution.top)) {
        metaEl.textContent = 'No data available';
        listEl.innerHTML = '';
        return;
    }

    const totalDistinct = distribution.total_distinct ?? 0;
    const totalCount = distribution.total_count ?? 0;
    metaEl.textContent = `${totalDistinct} distinct values • ${totalCount} total`;

    listEl.innerHTML = '';
    distribution.top.forEach(item => {
        const li = document.createElement('li');
        li.className = 'stat-item';
        li.innerHTML = `
            <span class="stat-label">${escapeHtml(item.name)}</span>
            <span class="stat-count">${item.count}</span>
        `;
        listEl.appendChild(li);
    });
}

function displayStudiesWithIssues(studies) {
    const container = document.getElementById('studies-list');
    container.innerHTML = '';
    
    // Sort studies by number of errors (descending)
    const sortedStudies = studies.sort((a, b) => {
        const aErrors = a.errors ? a.errors.length : 0;
        const bErrors = b.errors ? b.errors.length : 0;
        return bErrors - aErrors;
    });
    
    sortedStudies.forEach(study => {
        const studyCard = createStudyCard(study);
        container.appendChild(studyCard);
    });
}

function createStudyCard(study) {
    const card = document.createElement('div');
    card.className = 'study-card';
    
    const errorCount = study.errors ? study.errors.length : 0;
    const warningCount = study.warnings ? study.warnings.length : 0;
    
    let html = `
        <div class="study-header">
            <h3>${escapeHtml(study.name)}</h3>
            <div class="study-badges">
                ${errorCount > 0 ? `<span class="badge badge-error">${errorCount} error${errorCount !== 1 ? 's' : ''}</span>` : ''}
                ${warningCount > 0 ? `<span class="badge badge-warning">${warningCount} warning${warningCount !== 1 ? 's' : ''}</span>` : ''}
            </div>
        </div>
        <div class="study-info">
            <span><strong>File:</strong> ${escapeHtml(study.file)}</span>
    `;
    
    if (study.rows !== null && study.rows !== undefined) {
        html += `<span><strong>Dimensions:</strong> ${study.rows} rows × ${study.cols} columns</span>`;
    }
    
    html += '</div>';
    
    // Display errors
    if (errorCount > 0) {
        html += '<div class="issues-container">';
        html += '<h4 class="issue-title error-title">❌ Errors:</h4>';
        html += '<ul class="issue-list">';
        study.errors.forEach(error => {
            html += `<li class="issue-item error-item">${escapeHtml(error)}</li>`;
        });
        html += '</ul></div>';
    }
    
    // Display warnings
    if (warningCount > 0) {
        html += '<div class="issues-container">';
        html += '<h4 class="issue-title warning-title">⚠️ Warnings:</h4>';
        html += '<ul class="issue-list">';
        study.warnings.forEach(warning => {
            html += `<li class="issue-item warning-item">${escapeHtml(warning)}</li>`;
        });
        html += '</ul></div>';
    }
    
    card.innerHTML = html;
    return card;
}

function formatDate(timestamp) {
    if (!timestamp) return 'N/A';
    const date = new Date(timestamp);
    return date.toLocaleString('en-US', {
        year: 'numeric',
        month: 'short',
        day: 'numeric',
        hour: '2-digit',
        minute: '2-digit',
        timeZoneName: 'short'
    });
}

function escapeHtml(text) {
    if (text === null || text === undefined) return '';
    const div = document.createElement('div');
    div.textContent = text.toString();
    return div.innerHTML;
}

function hideLoading() {
    document.getElementById('loading').style.display = 'none';
}

function showError(message) {
    document.getElementById('loading').style.display = 'none';
    document.getElementById('error').style.display = 'block';
    document.getElementById('error-text').textContent = message;
}

function initTabs() {
    const buttons = document.querySelectorAll('.tab-button');
    const panels = document.querySelectorAll('.tab-panel');

    if (!buttons.length || !panels.length) return;

    buttons.forEach(button => {
        button.addEventListener('click', () => {
            const tabName = button.getAttribute('data-tab');

            buttons.forEach(btn => btn.classList.remove('active'));
            panels.forEach(panel => panel.classList.remove('active'));

            button.classList.add('active');
            const panel = document.getElementById(`tab-${tabName}`);
            if (panel) {
                panel.classList.add('active');
            }
        });
    });
}

// Initialize when page loads
document.addEventListener('DOMContentLoaded', () => {
    initTabs();
    loadValidationResults();
});
