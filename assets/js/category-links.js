//* assets/js/category-links.js 

document.addEventListener('DOMContentLoaded', function() {
  // Wait for the page to fully load
  setTimeout(function() {
    // Find all category links in the sidebar
    const categoryLinks = document.querySelectorAll('.quarto-category a');
    
    categoryLinks.forEach(function(link) {
      // Make sure the link has an event listener
      link.addEventListener('click', function(e) {
        e.preventDefault();
        
        // Get the category name from the link
        const category = this.textContent.trim();
        
        // Create the URL with the category parameter
        const url = window.location.pathname + '?category=' + encodeURIComponent(category);
        
        // Navigate to the URL
        window.location.href = url;
      });
    });
    
    console.log('Enhanced category links: ' + categoryLinks.length);
  }, 1000); // Wait a second to ensure everything is loaded
});