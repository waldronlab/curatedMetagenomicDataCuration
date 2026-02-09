// Configuration
const VALIDATION_DATA_URL = 'validation_results.json';
const REPO_OWNER = 'waldronlab';
const REPO_NAME = 'curatedMetagenomicDataCuration';

// Store distribution data for re-sorting
const distributionData = {};

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
    // Calculate validation success percentage
    const totalFiles = data.summary.total_files || 0;
    const filesWithIssues = data.summary.files_with_issues || 0;
    const successfulFiles = totalFiles - filesWithIssues;
    const successPercentage = totalFiles > 0 ? Math.round((successfulFiles / totalFiles) * 100) : 0;

    // Update summary cards
    document.getElementById('overall-status').textContent = `${successPercentage}% Pass`;
    document.getElementById('overall-status').className = `status-badge status-${data.status.toLowerCase()}`;

    document.getElementById('total-files').textContent = totalFiles;
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
        displayIssuesSummaryTable(data.studies_with_issues);
        displayStudiesWithIssues(data.studies_with_issues);
        document.getElementById('issues-summary-section').style.display = 'block';
        document.getElementById('issues-section').style.display = 'block';
        document.getElementById('success-section').style.display = 'none';
    } else {
        document.getElementById('issues-summary-section').style.display = 'none';
        document.getElementById('issues-section').style.display = 'none';
        document.getElementById('success-section').style.display = 'block';
    }

    // Display harmonized metadata stats
    if (data.stats) {
        displayStats(data.stats);
    }
    
    // Show content and header tabs
    document.getElementById('content').style.display = 'block';
    const headerTabs = document.getElementById('header-tabs');
    if (headerTabs) headerTabs.style.display = '';
}

function displayStats(stats) {
    document.getElementById('total-studies').textContent = stats.total_studies ?? 0;
    document.getElementById('total-samples').textContent = stats.total_samples ?? 0;

    const distributions = stats.distributions || {};
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

    // Store original data for re-sorting
    distributionData[prefix] = distribution.top.slice();

    const totalDistinct = distribution.total_distinct ?? 0;
    const totalCount = distribution.total_count ?? 0;
    metaEl.textContent = `${totalDistinct} distinct values • ${totalCount} total`;

    // Default sort is by count (descending)
    renderDistributionList(prefix, 'count');
}

function renderDistributionList(prefix, sortBy) {
    const listEl = document.getElementById(`${prefix}-list`);
    const items = distributionData[prefix];
    if (!items) return;

    const sorted = [...items].sort((a, b) => {
        if (sortBy === 'value') {
            return String(a.name).localeCompare(String(b.name));
        }
        return b.count - a.count;
    });

    listEl.innerHTML = '';
    sorted.forEach(item => {
        const li = document.createElement('li');
        li.className = 'stat-item';
        li.innerHTML = `
            <span class="stat-label">${escapeHtml(item.name)}</span>
            <span class="stat-count">${item.count}</span>
        `;
        listEl.appendChild(li);
    });
}

function initSortToggles() {
    document.querySelectorAll('.stat-panel[data-dist]').forEach(panel => {
        const prefix = panel.getAttribute('data-dist');
        panel.querySelectorAll('.sort-btn').forEach(btn => {
            btn.addEventListener('click', () => {
                const sortBy = btn.getAttribute('data-sort');
                // Update active state within this panel
                panel.querySelectorAll('.sort-btn').forEach(b => b.classList.remove('active'));
                btn.classList.add('active');
                renderDistributionList(prefix, sortBy);
            });
        });
    });
}

function parseIssueMessages(messages) {
    const types = new Set();
    const columns = new Set();

    (messages || []).forEach(msg => {
        // "Missing required fields: col1, col2, col3"
        const missingMatch = msg.match(/^Missing required fields:\s*(.+)$/);
        if (missingMatch) {
            types.add('Missing required');
            missingMatch[1].split(/,\s*/).forEach(c => columns.add(c.trim()));
            return;
        }

        // "VALIDATION ERROR: ..."
        if (/^VALIDATION ERROR:/.test(msg)) {
            types.add('Validation error');
            return;
        }

        // "Field '<col>' expected type '<expected>' but found '<actual>'"
        const typeMatch = msg.match(/^Field '([^']+)'.*expected type/);
        if (typeMatch) {
            types.add('Type mismatch');
            columns.add(typeMatch[1]);
            return;
        }

        // "Field '<col>' ... invalid values:"
        const enumMatch = msg.match(/^Field '([^']+)'.*invalid values?:/);
        if (enumMatch) {
            types.add('Invalid values');
            columns.add(enumMatch[1]);
            return;
        }

        // "Field '<col>' ... not matching pattern:"
        const patternMatch = msg.match(/^Field '([^']+)'.*not matching pattern/);
        if (patternMatch) {
            types.add('Pattern violation');
            columns.add(patternMatch[1]);
            return;
        }

        // "Row N: '<col>' has a value but '<col2>' is missing"
        const combinedMissing = msg.match(/^Row \d+: '([^']+)' has a value but '([^']+)' is missing/);
        if (combinedMissing) {
            types.add('Missing paired field');
            columns.add(combinedMissing[1]);
            columns.add(combinedMissing[2]);
            return;
        }

        // "Row N: '<col>' has N value(s) but '<col2>' has N value(s)"
        const combinedCount = msg.match(/^Row \d+: '([^']+)' has \d+ value\(s\) but '([^']+)'/);
        if (combinedCount) {
            types.add('Count mismatch');
            columns.add(combinedCount[1]);
            columns.add(combinedCount[2]);
            return;
        }

        // "Row N: '<col>' has invalid values:"
        const rowEnum = msg.match(/^Row \d+: '([^']+)' has invalid values?:/);
        if (rowEnum) {
            types.add('Invalid values');
            columns.add(rowEnum[1]);
            return;
        }

        // "Row N: '<col>' value ... does not match pattern"
        const rowPattern = msg.match(/^Row \d+: '([^']+)' value .* does not match pattern/);
        if (rowPattern) {
            types.add('Pattern violation');
            columns.add(rowPattern[1]);
            return;
        }

        // Fallback — try to grab Field '<col>'
        const fallback = msg.match(/Field '([^']+)'/);
        if (fallback) {
            types.add('Other');
            columns.add(fallback[1]);
        } else {
            types.add('Other');
        }
    });

    return { types, columns };
}

function displayIssuesSummaryTable(studies) {
    const tbody = document.getElementById('issues-summary-body');
    tbody.innerHTML = '';

    const sorted = [...studies].sort((a, b) => {
        const aE = a.errors ? a.errors.length : 0;
        const bE = b.errors ? b.errors.length : 0;
        return bE - aE;
    });

    sorted.forEach(study => {
        const allMessages = [
            ...(study.errors || []),
            ...(study.warnings || [])
        ];
        const { types } = parseIssueMessages(allMessages);

        const errorCount = study.errors ? study.errors.length : 0;
        const warningCount = study.warnings ? study.warnings.length : 0;
        const samples = (study.rows !== null && study.rows !== undefined) ? study.rows : '—';

        const typeBadges = [...types].map(t => {
            const cls = errorCount > 0 ? 'badge-error' : 'badge-warning';
            return `<span class="badge ${cls}">${escapeHtml(t)}</span>`;
        }).join(' ');

        const tr = document.createElement('tr');
        tr.innerHTML = `
            <td class="study-name-cell">${escapeHtml(study.name)}</td>
            <td class="num-cell">${samples}</td>
            <td class="num-cell">${errorCount > 0 ? `<span class="error-count">${errorCount}</span>` : '0'}</td>
            <td class="num-cell">${warningCount > 0 ? `<span class="warning-count">${warningCount}</span>` : '0'}</td>
            <td>${typeBadges}</td>
        `;
        tbody.appendChild(tr);
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
    initSortToggles();
    loadValidationResults();
});
