
const portfolioData = {
    name: "Chris Dance",
    email: "chrisdance66@outlook.com",
    social: {
        github: "https://github.com/AlmostNonTrivial",
        youtube: "https://www.youtube.com/@almost_on_trivial",
        linkedin: "https://www.linkedin.com/in/chris-dance-98620b20b/"
    },
    projects: [
        {
            title: "SQL",
            description: "An educational SQL engine based on SQLite, with query-compilation, a bytecode VM, and B+Tree storage engine, built from scratch",

            youtubeId: "d9a3attUq3o",
            githubLink: "https://github.com/AlmostNonTrivial/SQL-FromScratch"
        },
        {
            title: "Multiplayer Game",
            description: "An educational multiplayer game demonstrating client-server architecture with UDP networking, built from scratch",
            youtubeId: "UC02WaOcnAQ",
            githubLink: "https://github.com/AlmostNonTrivial/CoD-Multiplayer-FromScratch."
        },
        {
            title: "Optimizing Compiler",
            description: `An educational C compiler demonstrating key pipeline stages: parsing, type-checking, AST lowering, SSA construction,
            SSA optimizations, graph-color based register allocation, and assembly emission, built from scratch. It's web based, so open up devtools and check it out!
            `,
            imageUrl: "compiler_image.png",
            githubLink: "https://github.com/AlmostNonTrivial/toy_optimizing_compiler",
        }
    ]
};

function createSocialLink(platform, url) {
    const link = document.createElement('a');
    link.className = 'social-link';
    link.textContent = platform;
    link.target = '_blank';
    if (platform === 'email') {
        link.addEventListener('click', (e) => {
            e.preventDefault();
            navigator.clipboard.writeText(url).then(() => {
                const originalText = link.textContent;
                link.textContent = 'copied!';
                setTimeout(() => {
                    link.textContent = originalText;
                }, 1000);
            });
        });
    } else {

        link.href = url;

    }
    return link;
}

function createProjectCard(project, styleClass) {
    const card = document.createElement('div');
    card.className = `project-card ${styleClass}`;

    const mediaContainer = document.createElement('div');
    mediaContainer.className = 'project-image';

    if (project.youtubeId) {
        const iframe = document.createElement('iframe');
        iframe.src = `https://www.youtube.com/embed/${project.youtubeId}`;
        iframe.frameBorder = '0';
        iframe.allow = 'accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture';
        iframe.allowFullscreen = true;
        iframe.style.width = '100%';
        iframe.style.height = '100%';
        mediaContainer.appendChild(iframe);
    } else if (project.imageUrl) {
        mediaContainer.style.backgroundImage = `url(${project.imageUrl})`;
    }

    const textContainer = document.createElement('div');

    const title = document.createElement('h3');
    title.textContent = project.title;

    const desc = document.createElement('p');
    desc.textContent = project.description;

    const link = document.createElement('a');
    link.href = project.githubLink;
    link.textContent = 'View on GitHub';
    link.target = '_blank';

    textContainer.appendChild(title);
    textContainer.appendChild(desc);
    textContainer.appendChild(link);

    card.appendChild(mediaContainer);
    card.appendChild(textContainer);

    return card;
}

function createStyle3() {
    const container = document.createElement('div');
    container.className = 'style-container style-3';

    const topLeft = document.createElement('div');
    topLeft.className = 'corner top-left';
    const name = document.createElement('h1');
    name.textContent = portfolioData.name;
    topLeft.appendChild(name);

    const topRight = document.createElement('div');
    topRight.className = 'corner top-right';
    topRight.appendChild(createSocialLink('github', portfolioData.social.github));
    topRight.appendChild(createSocialLink('youtube', portfolioData.social.youtube));
    topRight.appendChild(createSocialLink('linkedin', portfolioData.social.linkedin));
    topRight.appendChild(createSocialLink('email', portfolioData.email));

    const content = document.createElement('div');
    content.className = 'content-3';

    const about = document.createElement('div');
    about.className = 'about-3';
    about.textContent = portfolioData.about;

    const projects = document.createElement('div');
    projects.className = 'projects-3';
    portfolioData.projects.forEach(proj => {
        const card = createProjectCard(proj, 'card-3');
        if (proj.title === "Optimizing Compiler") {
            card.firstChild.classList.add('compiler-card');
            card.firstChild.addEventListener('click', () => {
                window.location = 'compiler.html';
            })
        }
        projects.appendChild(card);
    });



    content.appendChild(projects);

    container.appendChild(topLeft);
    container.appendChild(topRight);
    container.appendChild(content);

    return container;
}

function init() {
    const scrollContainer = document.createElement('div');
    scrollContainer.id = 'scroll-container';

    scrollContainer.appendChild(createStyle3());

    document.body.appendChild(scrollContainer);
}

init();