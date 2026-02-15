# Ki Yieung Goh - Personal Portfolio

A clean, professional portfolio website showcasing product management work, personal projects, hackathon achievements, data analysis, and publications.

🌐 **Live Site**: [kyiyeunggoh.github.io](https://kyiyeunggoh.github.io)

## 🎨 Design Features

- **Dark cyberpunk aesthetic** with terminal green (`#00ff88`) accents
- **8-bit pixel cat** that bounces and reacts to scrolling
- **Glitch effect** on name for personality
- **Animated cards** with hover effects
- **Grid background pattern** for depth
- **Fully responsive** - works on all devices

## 📁 File Structure

```
kyiyeunggoh.github.io/
├── index.html              # Main HTML file
├── assets/
│   ├── css/
│   │   └── main.css        # All styles
│   └── js/
│       └── main.js         # Interactive features
├── images/                 # (Optional) For logos and profile pic
│   ├── profile.jpg         # Your profile picture
│   └── logos/              # Product logos
└── README.md               # This file
```

## 🚀 Quick Start

### Option 1: Direct Upload to GitHub Pages

1. **Clone or download this repository**
2. **Upload all files to your GitHub repository** at `kyiyeunggoh/kyiyeunggoh.github.io`
3. **Enable GitHub Pages** in repository settings:
   - Go to Settings → Pages
   - Source: Deploy from branch `main` (or `master`)
   - Folder: `/ (root)`
4. **Wait 1-2 minutes** for deployment
5. **Visit** `https://kyiyeunggoh.github.io`

### Option 2: Local Development

```bash
# Clone the repository
git clone https://github.com/kyiyeunggoh/kyiyeunggoh.github.io.git
cd kyiyeunggoh.github.io

# Open index.html in your browser
# Or use a local server (recommended):
python -m http.server 8000
# Visit: http://localhost:8000
```

## 🖼️ Adding Your Images

### Profile Picture

1. Add your profile picture to `images/profile.jpg`
2. In `index.html`, uncomment and update line 19:
```html
<img src="images/profile.jpg" alt="Ki Yieung Goh">
```
3. Remove or comment out the placeholder:
```html
<!-- <div class="profile-pic-placeholder">KG</div> -->
```

### Product Logos

1. Create folder: `images/logos/`
2. Add logo images (PNG or JPG recommended, 256x256px or larger)
3. In `index.html`, find each product and uncomment the image line:

**ForkIt** (around line 50):
```html
<img src="images/logos/forkit.png" alt="ForkIt logo">
```

**WokenApp** (around line 62):
```html
<img src="images/logos/wokenapp.png" alt="WokenApp logo">
```

**Fomove** (around line 74):
```html
<img src="images/logos/fomove.png" alt="Fomove logo">
```

**Hawkernomics** (around line 96):
```html
<img src="images/logos/hawkernomics.png" alt="Hawkernomics logo">
```

**Tamagotcha** (around line 108):
```html
<img src="images/logos/tamagotcha.png" alt="Tamagotcha logo">
```

**Sentry** (around line 120):
```html
<img src="images/logos/sentry.png" alt="Sentry logo">
```

**ReadLiao** (around line 132):
```html
<img src="images/logos/readliao.png" alt="ReadLiao logo">
```

Then remove or comment out the placeholder spans:
```html
<!-- <span class="product-logo-placeholder">FK</span> -->
```

## 🎯 Customizing Content

### Update Personal Information

Edit `index.html` to change:
- **Name**: Line 23
- **Title**: Line 24  
- **Tagline**: Line 25
- **LinkedIn URL**: Line 27
- **GitHub URL**: Line 28

### Add New Projects

Copy this template and paste into the appropriate section:

```html
<article class="project-card">
    <div class="project-header">
        <div class="project-title-wrapper">
            <div class="product-logo">
                <img src="images/logos/YOUR_LOGO.png" alt="Project logo">
            </div>
            <h3><a href="YOUR_URL" target="_blank" class="ext-link">Project Name</a></h3>
        </div>
        <span class="project-tag">Category</span>
    </div>
    <p class="project-description">Description of your project here.</p>
</article>
```

### Modify Colors

Edit `assets/css/main.css` (lines 8-16):

```css
:root {
    --color-bg: #0a0a0a;           /* Background color */
    --color-surface: #1a1a1a;      /* Card background */
    --color-text: #e8e8e8;         /* Main text color */
    --color-text-muted: #888888;   /* Secondary text */
    --color-accent: #00ff88;       /* Main accent (green) */
    --color-border: #2a2a2a;       /* Border color */
}
```

## 🐱 The Pixel Cat

The bouncing cat in the bottom-right corner:
- **Bounces continuously** - creates liveliness
- **Moves as you scroll** - subtle upward movement
- **Click it** - waves at you!
- Matches the terminal green aesthetic

## ✨ Interactive Features

- ✅ Smooth scroll animations
- ✅ Cards slide on hover
- ✅ External link indicators (↗)
- ✅ Glitch effect on name
- ✅ Scroll-triggered reveals
- ✅ Animated pixel cat
- ✅ Responsive design
- ✅ Accessibility features

## 📱 Browser Support

- ✅ Chrome (last 2 versions)
- ✅ Firefox (last 2 versions)
- ✅ Safari (last 2 versions)
- ✅ Edge (last 2 versions)
- ✅ Mobile browsers

## 🔧 Tech Stack

- **HTML5** - Semantic markup
- **CSS3** - Modern styling with Grid and Flexbox
- **Vanilla JavaScript** - No dependencies!
- **Google Fonts** - Space Mono & Newsreader

## 📊 Performance

Target Lighthouse scores:
- Performance: 95+
- Accessibility: 95+
- Best Practices: 100
- SEO: 95+

## 🐛 Troubleshooting

**Site not loading?**
- Check GitHub Pages is enabled in repo settings
- Wait 1-2 minutes for deployment
- Clear browser cache

**Images not showing?**
- Verify image paths are correct
- Check image files are in `images/` folder
- Image names are case-sensitive

**Cat not bouncing?**
- Check `assets/js/main.js` is loaded
- Open browser console for errors
- Try hard refresh (Cmd/Ctrl + Shift + R)

## 📧 Contact

- **LinkedIn**: [linkedin.com/in/kyiyeunggoh](https://www.linkedin.com/in/kyiyeunggoh/)
- **GitHub**: [github.com/kyiyeunggoh](https://github.com/kyiyeunggoh)
- **Twitter**: [@kyigoh](https://twitter.com/kyigoh)

## 📄 License

This portfolio site design is for personal use. Feel free to use as inspiration for your own portfolio!

---

**Last Updated**: February 2025  
**Version**: 2.0 - Complete Revamp with Cyberpunk Aesthetic
