// ===================================
// SCROLL REVEAL ANIMATION
// ===================================

const reveals = document.querySelectorAll('.reveal');

function checkReveal() {
    reveals.forEach(element => {
        const elementTop = element.getBoundingClientRect().top;
        const windowHeight = window.innerHeight;
        
        if (elementTop < windowHeight - 100) {
            element.classList.add('active');
        }
    });
}

window.addEventListener('scroll', checkReveal);
checkReveal(); // Check on load

// ===================================
// SMOOTH SCROLL FOR ANCHOR LINKS
// ===================================

document.querySelectorAll('a[href^="#"]').forEach(anchor => {
    anchor.addEventListener('click', function (e) {
        e.preventDefault();
        const target = document.querySelector(this.getAttribute('href'));
        if (target) {
            target.scrollIntoView({ behavior: 'smooth' });
        }
    });
});

// ===================================
// ANIMATED CAT THAT REACTS TO SCROLLING
// ===================================

const pixelCat = document.querySelector('.pixel-cat');
let lastScrollY = window.scrollY;
let scrollTimeout;

window.addEventListener('scroll', () => {
    const currentScrollY = window.scrollY;
    
    // Add scrolling class for animation
    pixelCat.classList.add('scrolling');
    
    // Clear previous timeout
    clearTimeout(scrollTimeout);
    
    // Remove scrolling class after animation
    scrollTimeout = setTimeout(() => {
        pixelCat.classList.remove('scrolling');
    }, 500);
    
    // Move cat based on scroll position (but keep it in view)
    const scrollPercent = currentScrollY / (document.documentElement.scrollHeight - window.innerHeight);
    const maxMove = window.innerHeight - 200; // Keep cat in viewport
    const newBottom = 32 + (scrollPercent * maxMove * 0.3); // Subtle movement
    
    pixelCat.style.bottom = `${Math.min(newBottom, window.innerHeight - 100)}px`;
    
    lastScrollY = currentScrollY;
});

// ===================================
// CAT WAVES WHEN CLICKED
// ===================================

pixelCat.style.cursor = 'pointer';
pixelCat.style.pointerEvents = 'all';
pixelCat.addEventListener('click', () => {
    pixelCat.style.transform = 'rotate(15deg)';
    setTimeout(() => {
        pixelCat.style.transform = 'rotate(-15deg)';
        setTimeout(() => {
            pixelCat.style.transform = 'rotate(0deg)';
        }, 150);
    }, 150);
});

// ===================================
// EXTERNAL LINKS - ADD SECURITY ATTRIBUTES
// ===================================

const externalLinks = document.querySelectorAll('a[target="_blank"]');
externalLinks.forEach(link => {
    link.setAttribute('rel', 'noopener noreferrer');
});

// ===================================
// PERFORMANCE - LOG PAGE LOAD TIME (DEV ONLY)
// ===================================

if (window.location.hostname === 'localhost' || window.location.hostname === '127.0.0.1') {
    window.addEventListener('load', function() {
        if (performance.timing) {
            const perfData = performance.timing;
            const pageLoadTime = perfData.loadEventEnd - perfData.navigationStart;
            console.log(`Page load time: ${pageLoadTime}ms`);
        }
    });
}

// ===================================
// ACCESSIBILITY - SKIP TO MAIN CONTENT
// ===================================

const skipLink = document.createElement('a');
skipLink.href = '#main';
skipLink.textContent = 'Skip to main content';
skipLink.className = 'skip-link';
skipLink.style.cssText = `
    position: absolute;
    top: -40px;
    left: 0;
    background: #00ff88;
    color: #0a0a0a;
    padding: 0.5rem 1rem;
    text-decoration: none;
    z-index: 100;
    font-family: 'Space Mono', monospace;
`;

skipLink.addEventListener('focus', function() {
    this.style.top = '0';
});

skipLink.addEventListener('blur', function() {
    this.style.top = '-40px';
});

document.body.insertBefore(skipLink, document.body.firstChild);

// Add id to main for skip link
const mainElement = document.querySelector('main');
if (mainElement && !mainElement.id) {
    mainElement.id = 'main';
}
