'Fire ball on a pendulum reaching portals. Until we get a better name.
'-----------------------------------------------------------------------------------------------------
'All assets can be downloaded from https://github.com/FellippeHeitor/Pendulum
'
'Original game concept by Voodoo/h8games: https://itunes.apple.com/us/app/fire-rides/id1279296287?mt=8
'
'Source audio material:
'    Fireball whoosh sound: https://freesound.org/s/248116/
'    Thump + Match sizzle sound: https://freesound.org/s/368606/ + https://freesound.org/s/237406/
'    Glass sound: https://freesound.org/s/371091/
'    Falling particles: https://freesound.org/s/408343/
'
OPTION _EXPLICIT
CONST true = -1, false = 0
CONST debug = false

DIM SHARED gameWidth AS INTEGER, gameHeight AS INTEGER
DIM SHARED arenaWidth AS INTEGER, arenaHeight AS INTEGER
DIM SHARED blockSize AS INTEGER
DIM SHARED bgWidth AS INTEGER, bgHeight AS INTEGER
DIM SHARED gameScreen AS LONG
DIM SHARED level AS LONG, camera AS SINGLE
DIM SHARED respawnOffset AS INTEGER
DIM SHARED ball.radius, ball.y.velocity
DIM SHARED ball.y.acceleration, ball.x.velocity, ball.x.acceleration
DIM SHARED ball.origin.x, ball.origin.y, ball.x, ball.y
DIM SHARED ball.mass
DIM SHARED level.r AS _UNSIGNED _BYTE, level.g AS _UNSIGNED _BYTE, level.b AS _UNSIGNED _BYTE
DIM SHARED g AS SINGLE, k AS LONG
DIM SHARED started AS _BYTE, p AS LONG, tx, ty, madeIt AS _BYTE
DIM SHARED cameraCenter AS SINGLE, cameraCenterY AS SINGLE, cameraY AS SINGLE
DIM SHARED timerSet AS _BYTE, m$, levelStarted AS SINGLE, levelSet AS SINGLE, finished AS _BYTE
DIM SHARED diff.y, diff.x, ball.angle, ball.arm, timeFinished AS SINGLE
DIM SHARED t.m$, mag, showBG AS _BYTE, blockLoopStart AS LONG
DIM SHARED ballHit AS _BYTE, glowRadius AS INTEGER
DIM SHARED ellipsisPlot AS INTEGER, waitForRelease AS _BYTE, willRespawn AS _BYTE
DIM SHARED gravitationalFloat AS SINGLE, g.angle AS SINGLE
DIM SHARED bigPortal AS LONG, smallPortal AS LONG
DIM SHARED frameRate AS INTEGER, frameRateRestoreTimer AS SINGLE
DIM SHARED respawnDelay AS SINGLE, hideRed AS SINGLE
DIM SHARED largeFont AS LONG, smallFont AS LONG
DIM SHARED prev.m$
DIM SHARED whooshSound AS LONG, fireOutSound AS LONG
DIM SHARED glassSound AS LONG, particlesSound AS LONG
DIM SHARED nextTetherPoint.x AS LONG, nextTetherPoint.y AS LONG

CONST maxGravitationalFloat = 20
CONST maxGlowRadius = 21
CONST arenaRatio.x = 15
CONST arenaRatio.y = 3

gameWidth = 900
gameHeight = 650
cameraCenter = 3
cameraCenterY = 2
frameRate = 60
respawnDelay = 1.5
blockSize = 100

gameScreen = _NEWIMAGE(gameWidth, gameHeight, 32)
SCREEN gameScreen
COLOR , _RGBA32(0, 0, 0, 0)
arenaWidth = gameWidth * arenaRatio.x
arenaHeight = gameHeight * arenaRatio.y
bgWidth = gameWidth * (arenaRatio.x * 2)
bgHeight = arenaHeight
showBG = true

'largeFont = _LOADFONT("cour.ttf", 40, "monospace")
IF largeFont = 0 THEN largeFont = 16
'smallFont = _LOADFONT("cour.ttf", 20, "monospace")
IF smallFont = 0 THEN smallFont = 16

whooshSound = _SNDOPEN("assets/whoosh.ogg")
fireOutSound = _SNDOPEN("assets/fireout.ogg")
glassSound = _SNDOPEN("assets/glass.ogg")
particlesSound = _SNDOPEN("assets/particles.ogg")

CONST FIRE = 1
CONST SMOKE = 2
CONST SPARK = 3
CONST GRAVITATOR = 4
CONST GRAVITATORCELL = 5
CONST BUSTEDCELL = 6

TYPE newItem
    x AS SINGLE
    y AS SINGLE
    w AS SINGLE
    h AS SINGLE
    yAcc AS SINGLE
    yVel AS SINGLE
    xAcc AS SINGLE
    xVel AS SINGLE
    a AS SINGLE
    b AS SINGLE
    minA AS SINGLE
    maxA AS SINGLE
    minb AS SINGLE
    maxB AS SINGLE
    multA AS _BYTE
    multB AS _BYTE
    kind AS INTEGER
    color AS _UNSIGNED LONG
    red AS _UNSIGNED _BYTE
    green AS _UNSIGNED _BYTE
    blue AS _UNSIGNED _BYTE
    active AS _BYTE
    parent AS LONG
    generation AS INTEGER
    maxGeneration AS INTEGER
END TYPE

CONST maxBGDecoration = 50
REDIM SHARED bg(1 TO maxBGDecoration) AS newItem
REDIM SHARED block(200) AS newItem
REDIM SHARED particle(5000) AS newItem
REDIM SHARED deathNote(1 TO 1) AS STRING, deathNoteIndex AS INTEGER
DIM SHARED totalBlocks AS LONG, totalSparks AS LONG
DIM thisParticle AS LONG

CONST maxRND = 500000
DIM SHARED rndTable(1 TO maxRND) AS SINGLE
DIM SHARED rndSeed AS LONG, rndIndex AS LONG

RANDOMIZE 4
DIM i&
FOR i& = 1 TO maxRND
    rndTable(i&) = noise(i&, 0, 0)
NEXT

RANDOMIZE TIMER

i& = 0
RESTORE deathNotes
DO
    READ m$
    IF m$ = "-" THEN EXIT DO
    i& = i& + 1
    IF i& > UBOUND(deathNote) THEN REDIM _PRESERVE deathNote(1 TO i&) AS STRING
    deathNote(i&) = m$
LOOP
IF i& = 0 THEN deathNote(1) = "OUCH!"

deathNotes:
DATA "OUCH!","THAT MUST HURT...","WASTED","CAN'T TOUCH THIS!","THE FLOOR IS LAVA. WATER. I MEAN WATER.","-"

DIM SHARED state AS _BYTE
CONST HALTED = 0
CONST FALLING = 1
CONST SWINGING = 2

DIM vr AS DOUBLE
DIM vt AS DOUBLE

ellipsisPlot = 63

level = 1
setRand level
generateArena
ball.radius = 20
ball.x = ball.radius * 1.5
ball.y = respawnOffset 'arenaHeight / respawnOffset
cameraY = ball.y
ball.mass = 1
levelSet = TIMER

deathNoteIndex = _CEIL(RND * UBOUND(deathNote))

DO
    IF _KEYDOWN(13) THEN
        IF NOT waitForRelease AND NOT willRespawn THEN
            IF state <> SWINGING THEN
                IF whooshSound > 0 THEN _SNDPLAYCOPY whooshSound
                state = SWINGING
                started = true
                IF timerSet = false THEN
                    timerSet = true
                    levelStarted = TIMER
                END IF

                ball.origin.x = nextTetherPoint.x
                ball.origin.y = nextTetherPoint.y
                IF ball.origin.x > arenaWidth THEN
                    finished = true
                END IF

                ball.arm = dist(ball.x, ball.y, ball.origin.x, ball.origin.y)

                diff.y = ball.y - ball.origin.y
                diff.x = ball.x - ball.origin.x
                ball.angle = _ATAN2(diff.x, diff.y)

                ' Eliminate radial velocity component but keep the tangential component.
                vt = ball.x.velocity * -COS(ball.angle) + ball.y.velocity * SIN(ball.angle)
                ball.x.velocity = vt * -COS(ball.angle)
                ball.y.velocity = vt * SIN(ball.angle)

            END IF
        END IF
    ELSE
        IF waitForRelease THEN
            waitForRelease = false
        ELSE
            IF state = SWINGING AND NOT finished THEN
                state = FALLING
            END IF
        END IF
    END IF

    k = _KEYHIT
    IF k = 9 AND _KEYDOWN(100304) = false THEN GOTO setNewLevel
    IF k = 9 AND _KEYDOWN(100304) AND level > 1 THEN level = level - 2: GOTO setNewLevel
    IF k = ASC("B") OR k = ASC("b") THEN showBG = NOT showBG
    IF k = ASC("W") OR k = ASC("w") THEN ball.y = ball.y + 1
    IF k = ASC("S") OR k = ASC("s") THEN ball.y = ball.y - 1

    doPhysics

    IF ball.x - ball.radius > arenaWidth THEN madeIt = true

    doCamera

    IF willRespawn THEN LINE (0, 0)-STEP(_WIDTH, _HEIGHT), _RGBA32(0, 0, 0, 30), BF ELSE CLS
    IF showBG THEN
        drawBG
    END IF
    drawBlocks

    IF state = SWINGING THEN
        ThickLine ball.x + camera, ball.y + cameraY, ball.origin.x + camera, ball.origin.y + cameraY, _RGB32(0, 0, 0), 8
        ThickLine ball.x + camera, ball.y + cameraY, ball.origin.x + camera, ball.origin.y + cameraY, _RGB32(255, 255, 255), 4
    END IF

    IF NOT willRespawn THEN
        FOR p = 1 TO 30
            tx = ball.x + COS(p) * (RND * ball.radius)
            ty = ball.y + SIN(p) * (RND * ball.radius)
            thisParticle = addParticle(tx, ty, FIRE, 0)
        NEXT
        FOR p = ball.radius + glowRadius TO ball.radius + 1 STEP -1
            CircleFill ball.x + camera, ball.y + cameraY, p, _RGBA32(238, 216, 94, map(p, ball.radius + 1, ball.radius + glowRadius, 15, 5))
        NEXT
        CircleFill ball.x + camera, ball.y + cameraY, ball.radius, _RGBA32(255, 255, 255, 200)
    END IF

    doParticles

    IF state = HALTED THEN
        IF NOT timerSet THEN m$ = "Hold ENTER to start..." ELSE m$ = "Hold ENTER to continue..."
        IF willRespawn THEN
            m$ = deathNote(deathNoteIndex)
            LINE (0, 0)-(_WIDTH, _HEIGHT), _RGBA32(72, 0, 0, 180), BF
        ELSE
            IF TIMER - hideRed < respawnDelay / 2 THEN
                LINE (0, 0)-(_WIDTH, _HEIGHT), _RGBA32(72, 0, 0, map(TIMER - hideRed, 0, respawnDelay / 2, 180, 0)), BF
            END IF
        END IF
        _FONT largeFont
        _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(m$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2), m$
    END IF

    IF timerSet THEN
        m$ = STR$(TIMER - levelStarted)
        m$ = LEFT$(m$, INSTR(m$, ".") + 1)
        _FONT smallFont
        _PRINTSTRING (_WIDTH - _PRINTWIDTH(m$), _HEIGHT - _FONTHEIGHT), m$
    END IF

    IF finished THEN
        m$ = "You made it!"
        _FONT largeFont
        _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(m$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2), m$
    ELSE
        IF started THEN
            m$ = STR$(TIMER - levelStarted)
            m$ = LEFT$(m$, INSTR(m$, ".") + 1)
            _FONT smallFont
            _PRINTSTRING (_WIDTH - _PRINTWIDTH(m$), _HEIGHT - _FONTHEIGHT), m$
        END IF
    END IF

    _FONT smallFont
    m$ = "Small portals:" + STR$(smallPortal)
    _PRINTSTRING (0, 0), m$
    m$ = "Big portals:" + STR$(bigPortal)
    _PRINTSTRING (0, _FONTHEIGHT), m$
    m$ = "CameraY:" + STR$(cameraY) + " ball.y:" + STR$(ball.y)
    _PRINTSTRING (0, _FONTHEIGHT * 2), m$

    _DISPLAY
    IF frameRate < 60 AND TIMER - frameRateRestoreTimer > respawnDelay THEN
        frameRate = 60
        willRespawn = false
        hideRed = TIMER
    END IF
    _LIMIT frameRate

    IF ball.x - ball.radius > arenaWidth THEN madeIt = true

    IF madeIt THEN
        timeFinished = TIMER
        t.m$ = STR$(timeFinished - levelStarted)
        t.m$ = LEFT$(t.m$, INSTR(t.m$, ".") + 1)
        IF _KEYDOWN(13) THEN waitForRelease = true
        _FONT largeFont
        DO
            IF showBG THEN
                IF willRespawn THEN LINE (0, 0)-STEP(_WIDTH, _HEIGHT), _RGBA32(0, 0, 0, 30), BF
                drawBG
            ELSE
                IF willRespawn THEN LINE (0, 0)-STEP(_WIDTH, _HEIGHT), _RGBA32(0, 0, 0, 30), BF ELSE CLS
            END IF
            drawBlocks
            doParticles

            m$ = "You made it in" + t.m$ + " seconds!"
            _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(m$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2), m$
            m$ = "(hit ENTER)"
            _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(m$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2 + _FONTHEIGHT), m$

            IF _KEYDOWN(13) AND NOT waitForRelease THEN
                EXIT DO
            ELSEIF NOT _KEYDOWN(13) AND waitForRelease THEN
                waitForRelease = false
            END IF

            _DISPLAY
            _LIMIT 60
        LOOP

        setNewLevel:
        FOR p = 1 TO UBOUND(particle)
            particle(p).active = false
        NEXT
        level = level + 1
        setRand level
        generateArena
        finished = false
        madeIt = false
        glowRadius = 3
        camera = 0
        cameraY = 0
        ball.x = ball.radius
        ball.y = respawnOffset
        ball.y.acceleration = 0
        ball.y.velocity = 0
        ball.x.acceleration = 0
        ball.x.velocity = 0
        started = false
        state = HALTED
        timerSet = false
        levelSet = TIMER
    END IF
LOOP

SUB doPhysics
    DIM DeltaTime AS DOUBLE
    DIM GravConst AS DOUBLE
    DIM Tension AS DOUBLE
    DIM Theta AS DOUBLE
    DIM Mass AS DOUBLE
    DIM MagVel AS DOUBLE
    DIM L AS DOUBLE
    DIM vr AS DOUBLE
    DIM vt AS DOUBLE
    DeltaTime = 0.5
    GravConst = 1
    L = ball.arm
    Mass = ball.mass

    diff.y = ball.y - ball.origin.y
    diff.x = ball.x - ball.origin.x
    ball.angle = _ATAN2(diff.x, diff.y)
    Theta = ball.angle

    IF state = FALLING THEN
        ball.x.acceleration = 0
        ball.y.acceleration = GravConst
        ball.x.velocity = ball.x.velocity + DeltaTime * ball.x.acceleration
        ball.y.velocity = ball.y.velocity + DeltaTime * ball.y.acceleration

        'Damping:
        IF (ball.y.velocity < 0) THEN ball.y.velocity = ball.y.velocity * (1 - .025)
        ball.x.velocity = ball.x.velocity * (1 - .025)

        ball.y = ball.y + DeltaTime * ball.y.velocity
        ball.x = ball.x + DeltaTime * ball.x.velocity

    ELSEIF state = SWINGING THEN
        MagVel = SQR(ball.x.velocity ^ 2 + ball.y.velocity ^ 2)

        Tension = Mass * GravConst * COS(Theta) + Mass * (MagVel ^ 2) / L

        ball.x.acceleration = (-1 / Mass) * (Tension * SIN(Theta))
        ball.y.acceleration = (-1 / Mass) * (Tension * COS(Theta) - GravConst)

        ball.x.velocity = ball.x.velocity + DeltaTime * ball.x.acceleration
        ball.y.velocity = ball.y.velocity + DeltaTime * ball.y.acceleration

        ' Artificial boost:
        ' (i  ) Convert X-Y representation of velocity to R-Theta representation.
        ' (ii ) Boost along the tangential direction.
        ' (iii) De-boost in the radial direction (OPTIONAL).
        ' (iv ) Convert back to XY.

        vr = ball.x.velocity * SIN(ball.angle) + ball.y.velocity * COS(ball.angle)
        vt = ball.x.velocity * -COS(ball.angle) + ball.y.velocity * SIN(ball.angle)

        IF (ball.x.velocity > 0) THEN vt = vt * (1 + 0.015)
        vr = vr * (1 + 0.05)

        ball.x.velocity = vr * SIN(ball.angle) + vt * -COS(ball.angle)
        ball.y.velocity = vr * COS(ball.angle) + vt * SIN(ball.angle)

        ball.x = ball.x + DeltaTime * ball.x.velocity
        ball.y = ball.y + DeltaTime * ball.y.velocity
    END IF

    IF ball.y - ball.radius / 2 > arenaHeight OR ballHit THEN
        IF fireOutSound > 0 THEN _SNDPLAYCOPY fireOutSound
        state = HALTED
        deathNoteIndex = _CEIL(RND * UBOUND(deathNote))
        waitForRelease = true
        willRespawn = true
        slowMo
        glowRadius = 3
        DO
            ball.y = respawnOffset
            drawBlocks
            IF ballHit = false THEN EXIT DO
            ball.x = ball.x + blockSize / 2
        LOOP
        ball.y.acceleration = 0
        ball.y.velocity = 0
        ball.x.acceleration = 0
        ball.x.velocity = 0
    END IF
END SUB

FUNCTION checkCollision%% (x1!, y1!, r!, x!, y!, w!, h!)
    '//solution from https://jsfiddle.net/TappT/z5r21nu0/16/
    'function collisionNormal(_ball, _rect) {
    '  if(_ball.pos.x + _ball.r > _rect.pos.x &&
    '     _ball.pos.x - _ball.r < (_rect.pos.x + _rect.w) &&
    '     _ball.pos.y + _ball.r > _rect.pos.y &&
    '     _ball.pos.y - _ball.r < (_rect.pos.y + _rect.h))
    '  {
    checkCollision%% = (x1! + camera) + r! > x! AND _
                       (x1! + camera) - r! < (x! + w!) AND _
                       (y1! + cameraY) + r! > y! AND _
                       (y1! + cameraY) - r! < (y! + h!)
END FUNCTION

SUB doCamera
    DIM new.cameraY AS SINGLE

    IF ball.x + camera > _WIDTH / cameraCenter THEN
        camera = (_WIDTH / cameraCenter) - ball.x
    ELSEIF ball.x + camera < _WIDTH / cameraCenter THEN
        camera = _WIDTH / cameraCenter - ball.x
    END IF

    IF ball.y + cameraY > _HEIGHT / cameraCenterY THEN
        new.cameraY = (_HEIGHT / cameraCenterY) - ball.y
    ELSEIF ball.y + cameraY < _HEIGHT / cameraCenterY THEN
        new.cameraY = _HEIGHT / cameraCenterY - ball.y
    END IF

    IF state = HALTED THEN
        DIM a AS SINGLE
        a = ABS(cameraY - new.cameraY)
        IF a > 3 THEN a = 3
        IF a < 3 THEN a = 0
        IF cameraY > new.cameraY THEN
            cameraY = cameraY - a
        ELSEIF cameraY < new.cameraY THEN
            cameraY = cameraY + a
        ELSE
            cameraY = new.cameraY
        END IF
    ELSE
        cameraY = new.cameraY
    END IF

    IF camera > 0 THEN camera = 0
    IF camera < -(arenaWidth - _WIDTH(gameScreen)) THEN camera = -(arenaWidth - _WIDTH(gameScreen))

    IF cameraY > 0 THEN cameraY = 0
    IF cameraY < -(arenaHeight - _HEIGHT(gameScreen)) THEN cameraY = -(arenaHeight - _HEIGHT(gameScreen))
END SUB

FUNCTION dist! (x1!, y1!, x2!, y2!)
    dist! = _HYPOT((x2! - x1!), (y2! - y1!))
END FUNCTION

FUNCTION map! (value!, minRange!, maxRange!, newMinRange!, newMaxRange!)
    map! = ((value! - minRange!) / (maxRange! - minRange!)) * (newMaxRange! - newMinRange!) + newMinRange!
END FUNCTION

SUB CircleFill (CX AS LONG, CY AS LONG, R AS LONG, C AS _UNSIGNED LONG)
    'This sub from here: http://www.qb64.net/forum/index.php?topic=1848.msg17254#msg17254
    DIM Radius AS LONG
    DIM RadiusError AS LONG
    DIM X AS LONG
    DIM Y AS LONG

    Radius = ABS(R)
    RadiusError = -Radius
    X = Radius
    Y = 0

    IF Radius = 0 THEN PSET (CX, CY), C: EXIT SUB

    ' Draw the middle span here so we don't draw it twice in the main loop,
    ' which would be a problem with blending turned on.
    LINE (CX - X, CY)-(CX + X, CY), C, BF

    WHILE X > Y

        RadiusError = RadiusError + Y * 2 + 1

        IF RadiusError >= 0 THEN

            IF X <> Y + 1 THEN
                LINE (CX - Y, CY - X)-(CX + Y, CY - X), C, BF
                LINE (CX - Y, CY + X)-(CX + Y, CY + X), C, BF
            END IF

            X = X - 1
            RadiusError = RadiusError - X * 2

        END IF

        Y = Y + 1

        LINE (CX - X, CY - Y)-(CX + X, CY - Y), C, BF
        LINE (CX - X, CY + Y)-(CX + X, CY + Y), C, BF

    WEND

END SUB


SUB generateArena
    DIM i AS LONG, j AS LONG, k AS LONG, lastGravitator AS LONG
    DIM w AS SINGLE, h AS SINGLE, x AS SINGLE, y AS SINGLE, s1 AS SINGLE, s2 AS SINGLE
    DIM colorVariation AS INTEGER
    DIM thisParticle AS LONG, minimumDistance AS INTEGER

    level.r = getRND * 256
    level.g = getRND * 256
    level.b = getRND * 256

    _FONT largeFont
    m$ = "Level" + STR$(level)
    FOR k = 1 TO maxBGDecoration
        LINE (0, 0)-STEP(_WIDTH, _HEIGHT), _RGBA32(0, 0, 0, 20), BF
        bg(k).w = getRND * bgWidth
        bg(k).h = getRND * bgHeight
        bg(k).x = getRND * bgWidth - bg(k).w / 3
        bg(k).y = getRND * bgHeight - bg(k).h / 3
        colorVariation = -(getRND * 256)
        bg(k).red = level.r + colorVariation
        bg(k).green = level.g + colorVariation
        bg(k).blue = level.b + colorVariation
        bg(k).color = _RGB32(level.r + colorVariation, level.g + colorVariation, level.b + colorVariation)
        LINE (bg(k).x, bg(k).y)-STEP(bg(k).w, bg(k).h), bg(k).color, BF
        LINE (bg(k).x / (arenaRatio.x * 2), bg(k).y / arenaRatio.y)-STEP(bg(k).w / (arenaRatio.x * 2), bg(k).h / arenaRatio.y), bg(k).color, BF
        COLOR _RGB32(0, 0, 0), 0
        _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(m$) / 2 + 1, _HEIGHT / 2 - _FONTHEIGHT / 2 + 1), m$
        COLOR _RGB32(255, 255, 255), 0
        _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(m$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2), m$
        _DISPLAY
        _LIMIT 30
    NEXT

    totalBlocks = 0
    minimumDistance = 5

    FOR i = 0 TO arenaWidth STEP blockSize
        'top block
        x = x + .1
        IF x > _PI(2) THEN x = x - _PI(2)
        h = arenaHeight * ABS(SIN(x) / 6) * getRND
        s1 = h
        y = 0
        GOSUB addBlock

        'bottom block
        h = arenaHeight
        y = s1 + 500
        s2 = y
        GOSUB addBlock

        IF i = 0 THEN respawnOffset = s1 + ((s2 - s1) / 2)

        IF (s2 - s1) > 200 AND i - lastGravitator > blockSize * minimumDistance AND i < arenaWidth - blockSize * 2 THEN 'AND (s2 - s1) < 450
            'add gravitators between these blocks
            thisParticle = addParticle(i + blockSize / 2, s1 + _CEIL(getRND * (s2 - s1)), GRAVITATOR, 0)
            lastGravitator = i + blockSize / 2
            particle(thisParticle).a = 25
            particle(thisParticle).minA = 1
            particle(thisParticle).maxA = 100
            particle(thisParticle).multA = -1
            particle(thisParticle).b = 150
            particle(thisParticle).minb = 150
            particle(thisParticle).maxB = 150
            particle(thisParticle).multB = 1

            minimumDistance = 5 + INT(getRND * 10)
        END IF
    NEXT
    EXIT SUB

    addBlock:
    totalBlocks = totalBlocks + 1
    IF totalBlocks > UBOUND(block) THEN REDIM _PRESERVE block(UBOUND(block) + 99) AS newItem
    block(totalBlocks).x = i
    block(totalBlocks).y = y
    block(totalBlocks).h = h
    block(totalBlocks).w = blockSize
    IF totalBlocks MOD 2 = 0 THEN
        block(totalBlocks).color = block(totalBlocks - 1).color
        block(totalBlocks).red = block(totalBlocks - 1).red
        block(totalBlocks).green = block(totalBlocks - 1).green
        block(totalBlocks).blue = block(totalBlocks - 1).blue
    ELSE
        colorVariation = 75 - getRND * 150
        block(totalBlocks).red = level.r + colorVariation
        block(totalBlocks).green = level.g + colorVariation
        block(totalBlocks).blue = level.b + colorVariation
        block(totalBlocks).color = _RGB32(level.r + colorVariation, level.g + colorVariation, level.b + colorVariation)
    END IF
    RETURN

END SUB

SUB ThickLine (x, y, x1, y1, c AS _UNSIGNED LONG, t)
    DIM i AS SINGLE, xx AS SINGLE, yy AS SINGLE
    'This sub from http://www.qb64.net/forum/index.php?topic=1456.msg11548#msg11548
    FOR i = 0 TO 1 STEP .01
        xx = INT((x1 - x) * i) + x
        yy = INT((y1 - y) * i) + y
        CircleFill xx, yy, t \ 2, c
    NEXT
END SUB

SUB drawBlocks
    DIM margin, j AS LONG
    DIM i AS SINGLE, y AS SINGLE, h AS SINGLE
    DIM blockSize AS SINGLE
    DIM s1 AS INTEGER, s2 AS INTEGER

    margin = 3
    ballHit = false
    blockLoopStart = INT(ABS(camera) / (block(1).w / 2)) - 2
    IF blockLoopStart < 1 THEN blockLoopStart = 1
    FOR j = blockLoopStart TO totalBlocks
        i = block(j).x + camera
        IF i > _WIDTH(0) THEN EXIT FOR
        y = block(j).y + cameraY
        h = block(j).h
        blockSize = block(j).w
        IF TIMER - levelSet < respawnDelay THEN
            LINE (i, y - margin)-STEP(blockSize, h + margin), _RGBA32(0, 0, 0, map(TIMER - levelSet, 0, respawnDelay, 0, 255)), BF
            LINE (i + margin, y)-STEP(blockSize - (margin * 2), h - margin), _RGBA32(block(j).red, block(j).green, block(j).blue, map(TIMER - levelSet, 0, respawnDelay, 0, 255)), BF
        ELSE
            LINE (i, y - margin)-STEP(blockSize, h + margin), _RGB32(0, 0, 0), BF
            LINE (i + margin, y)-STEP(blockSize - (margin * 2), h - margin), block(j).color, BF
        END IF
        IF ballHit = false THEN
            ballHit = checkCollision(ball.x, ball.y, ball.radius, i, y - margin, blockSize, h + margin)
        END IF

        IF ball.x > block(j).x AND ball.x < block(j).x + block(j).w AND block(j).y = 0 THEN
            IF j + 4 <= UBOUND(block) THEN
                nextTetherPoint.x = block(j + 4).x + block(j + 4).w / 2
                nextTetherPoint.y = block(j + 4).y + block(j + 4).h
                s1 = block(j + 4).h
                s2 = s1 + 500
                respawnOffset = s1 + ((s2 - s1) / 2)
            ELSE
                nextTetherPoint.x = ball.x + _WIDTH(gameScreen) / 5
                nextTetherPoint.y = 0
            END IF
            IF nextTetherPoint.x < ball.x THEN
                nextTetherPoint.x = ball.x + _WIDTH(gameScreen) / 5
                nextTetherPoint.y = 0
            END IF
        END IF
    NEXT
END SUB

FUNCTION addParticle (x AS SINGLE, y AS SINGLE, kind AS INTEGER, parent AS LONG)
    DIM i AS LONG
    DIM newParticle AS LONG, thisParticle AS LONG
    DIM a AS SINGLE
    DIM s AS SINGLE
    STATIC previousParent AS LONG, childrenCount AS LONG

    FOR i = 1 TO UBOUND(particle)
        IF NOT particle(i).active THEN newParticle = i: EXIT FOR
    NEXT

    IF newParticle = 0 THEN
        newParticle = UBOUND(particle) + 1
        REDIM _PRESERVE particle(1 TO UBOUND(particle) + 99) AS newItem
    END IF

    particle(newParticle).x = x
    particle(newParticle).xVel = 0
    particle(newParticle).xAcc = 0
    particle(newParticle).y = y
    particle(newParticle).yVel = 0
    particle(newParticle).yAcc = 0
    particle(newParticle).kind = kind
    particle(newParticle).parent = parent
    particle(newParticle).generation = 0
    particle(newParticle).active = true

    a = RND * _PI(2)

    SELECT CASE kind
        CASE FIRE
            particle(newParticle).yVel = SIN(a) * 2
            particle(newParticle).xVel = COS(a) * 2
        CASE SPARK
            particle(newParticle).yVel = SIN(a) * 3
            particle(newParticle).xVel = COS(a) * 3
            particle(newParticle).maxGeneration = 25
        CASE SMOKE
            particle(newParticle).maxGeneration = 15
        CASE GRAVITATOR
            FOR a = 1 TO ellipsisPlot * 2
                thisParticle = addParticle(x + COS(a) * particle(newParticle).a, y + SIN(a) * particle(newParticle).b, GRAVITATORCELL, newParticle)
            NEXT
        CASE GRAVITATORCELL
            IF parent <> previousParent THEN
                previousParent = parent
                childrenCount = 0
            END IF
            childrenCount = childrenCount + 1
            IF childrenCount <= ellipsisPlot THEN
                particle(newParticle).red = 233
                particle(newParticle).green = 211
                particle(newParticle).blue = 72
            ELSE
                particle(newParticle).red = 105
                particle(newParticle).green = 216
                particle(newParticle).blue = 94
            END IF
    END SELECT

    addParticle = newParticle
END FUNCTION

SUB doParticles
    DIM i AS LONG, j AS LONG, k AS SINGLE
    DIM gravity AS SINGLE
    DIM g AS _UNSIGNED _BYTE, b AS _UNSIGNED _BYTE, a AS _UNSIGNED _BYTE
    DIM s AS SINGLE, thisParticle AS LONG

    gravity = .02

    g.angle = g.angle + .01
    IF g.angle > _PI(2) THEN g.angle = g.angle - _PI(2)
    gravitationalFloat = SIN(g.angle) * maxGravitationalFloat

    totalSparks = 0

    FOR i = 1 TO UBOUND(particle)
        IF particle(i).active THEN
            particle(i).generation = particle(i).generation + 1

            particle(i).yVel = particle(i).yVel + particle(i).yAcc
            particle(i).y = particle(i).y + particle(i).yVel

            particle(i).xVel = particle(i).xVel + particle(i).xAcc
            particle(i).x = particle(i).x + particle(i).xVel

            SELECT EVERYCASE particle(i).kind
                CASE FIRE
                    'process
                    IF particle(i).generation > 8 THEN
                        particle(i).kind = SMOKE
                        particle(i).maxGeneration = 15
                        particle(i).generation = 0
                        particle(i).yVel = 0
                        particle(i).xVel = 0
                        particle(i).yAcc = -.4
                        particle(i).xAcc = -.2
                    ELSE
                        SELECT EVERYCASE particle(i).generation
                            CASE 1, 2: g = 238: b = 177: a = 200
                            CASE 3
                                s = RND
                                IF s < .02 THEN thisParticle = addParticle(particle(i).x, particle(i).y, SPARK, i)
                            CASE 3, 4: g = 222: b = 89: a = 180
                            CASE 5, 6: g = 128: b = 50: a = 150
                            CASE 7, 8: g = 33: b = 0: a = 70
                        END SELECT
                        particle(i).color = _RGBA32(255, g, b, a)
                    END IF
                CASE SMOKE
                    'process
                    particle(i).color = _RGBA32(33, 17, 39, map(particle(i).generation, 1, particle(i).maxGeneration, 100, 0))
                    IF particle(i).generation > particle(i).maxGeneration THEN particle(i).active = false
                CASE SPARK
                    totalSparks = totalSparks + 1
                    'process
                    particle(i).yAcc = particle(i).yAcc + gravity
                    SELECT CASE particle(i).generation
                        CASE 1, 2: g = 238: b = 177
                        CASE 3, 4: g = 222: b = 89
                        CASE 5, 6: g = 128: b = 50
                        CASE 7, 8: g = 33: b = 0
                    END SELECT
                    a = map(particle(i).generation, 1, 15, 255, 0)
                    particle(i).color = _RGBA32(255, g, b, a)
                    IF particle(i).generation > particle(i).maxGeneration THEN particle(i).active = false

                    'show
                    FOR p = glowRadius TO 1 STEP -1
                        CircleFill particle(i).x + camera, particle(i).y + cameraY, p, _RGBA32(238, 216, 94, map(p, 1, glowRadius, 15, 5))
                    NEXT
                    CircleFill particle(i).x + camera, particle(i).y + cameraY, 1, particle(i).color
                CASE GRAVITATOR
                    'particle(i).a = particle(i).a + particle(i).multA
                    'IF particle(i).a > particle(i).maxA OR particle(i).a < particle(i).minA THEN
                    '    particle(i).multA = particle(i).multA * -1
                    '    particle(i).a = particle(i).a + particle(i).multA
                    'END IF
                    'particle(i).b = particle(i).b + particle(i).multB
                    'IF particle(i).b > particle(i).maxB OR particle(i).b < particle(i).minb THEN
                    '    particle(i).multB = particle(i).multB * -1
                    '    particle(i).b = particle(i).b + particle(i).multB
                    'END IF

                    k = 0
                    FOR j = i + 1 TO i + ellipsisPlot + 1
                        IF particle(j).parent = i THEN
                            k = k + .1
                            particle(j).x = particle(i).x + COS(k) * map(particle(j).x + camera, 0, _WIDTH(0), particle(i).minA, particle(i).maxA)
                            particle(j).y = particle(i).y + SIN(k) * particle(i).b
                        END IF
                    NEXT

                    k = 0
                    FOR j = i + ellipsisPlot + 1 TO i + ellipsisPlot * 2
                        IF particle(j).parent = i THEN
                            k = k + .1
                            particle(j).x = particle(i).x + COS(k) * map(particle(j).x + camera, 0, _WIDTH(0), particle(i).minA, particle(i).maxA / 3)
                            particle(j).y = particle(i).y + SIN(k) * particle(i).b / 4
                        END IF
                    NEXT

                    'ball VS gravitator collision detection:
                    IF debug THEN
                        LINE (camera + particle(i).x - ball.radius / 2, cameraY + particle(i).y - 180)-STEP(ball.radius, 0), _RGB32(255, 255, 255)
                        LINE (camera + particle(i).x - ball.radius / 2, cameraY + particle(i).y + 180)-STEP(ball.radius, 0), _RGB32(255, 255, 255)
                        LINE (camera + particle(i).x - ball.radius * 2, cameraY + particle(i).y - 30)-STEP(0, 60), _RGB32(255, 255, 255)
                        LINE (camera + particle(i).x + ball.radius * 2, cameraY + particle(i).y - 30)-STEP(0, 60), _RGB32(255, 255, 255)
                        CIRCLE (particle(i).x + camera, particle(i).y + cameraY + gravitationalFloat), ball.radius * 2.5, _RGB32(255, 255, 255)
                    END IF

                    IF state <> HALTED THEN
                        IF dist(ball.x, ball.y, particle(i).x, particle(i).y + gravitationalFloat) < ball.radius * 2.5 THEN
                            smallPortal = smallPortal + 1
                            IF glowRadius < maxGlowRadius THEN glowRadius = glowRadius + 3
                            particle(i).active = false
                            IF glassSound > 0 THEN _SNDPLAYCOPY glassSound

                            FOR j = i + 1 TO i + ellipsisPlot + 1
                                IF particle(j).parent = i THEN
                                    particle(j).kind = BUSTEDCELL
                                    particle(j).generation = 0
                                    particle(j).maxGeneration = 50
                                    particle(j).xVel = COS(RND * _PI(2)) '* 2
                                    particle(j).parent = -2
                                END IF
                            NEXT

                            FOR j = i + ellipsisPlot + 1 TO i + ellipsisPlot * 2
                                IF particle(j).parent = i THEN
                                    particle(j).kind = BUSTEDCELL
                                    particle(j).generation = 0
                                    particle(j).maxGeneration = 100
                                    particle(j).yVel = SIN(RND * _PI(2)) * 10
                                    DO
                                        particle(j).xVel = COS(RND * _PI(2)) * 10
                                    LOOP UNTIL SGN(particle(j).xVel) = 1
                                    particle(j).parent = -1
                                END IF
                            NEXT
                        ELSEIF ball.x > particle(i).x - ball.radius*2 AND _
                               ball.x < particle(i).x + ball.radius*2 AND _
                               ball.y > particle(i).y - 180 + gravitationalFloat AND _
                               ball.y < particle(i).y + 180 + gravitationalFloat THEN
                            bigPortal = bigPortal + 1
                            IF particlesSound > 0 THEN _SNDPLAYCOPY particlesSound
                            'glowRadius = 0
                            particle(i).active = false
                            FOR j = i + 1 TO i + ellipsisPlot * 2
                                IF particle(j).parent = i THEN
                                    particle(j).kind = BUSTEDCELL
                                    particle(j).generation = 0
                                    particle(j).maxGeneration = 50
                                    'particle(j).yVel = SIN(rnd * _PI(2)) * 10
                                    particle(j).xVel = COS(RND * _PI(2)) '* 2
                                    particle(j).parent = -2
                                END IF
                            NEXT
                        END IF
                    END IF
                CASE BUSTEDCELL
                    particle(i).yAcc = particle(i).yAcc + gravity
                    IF particle(i).generation > particle(i).maxGeneration THEN
                        particle(i).active = false
                    ELSE
                        IF particle(i).parent = -1 THEN
                            FOR j = 6 TO 4 STEP -1
                                CircleFill particle(i).x + camera, particle(i).y + cameraY + gravitationalFloat, 6, _RGBA32(particle(i).red, particle(i).green, particle(i).blue, map(j, 4, 6, 10, 25))
                            NEXT
                            CircleFill particle(i).x + camera, particle(i).y + cameraY + gravitationalFloat, 3, _RGB32(255, 255, 255)
                        ELSEIF particle(i).parent = -2 THEN
                            CircleFill particle(i).x + camera, particle(i).y + cameraY + gravitationalFloat, 3, _RGB32(160, 160, 160)
                        END IF
                    END IF
                CASE FIRE, SMOKE
                    'show
                    s = dist(particle(i).x, particle(i).y, ball.x, ball.y)
                    s = map(s, 0, ball.radius * 4, 9, 2)
                    IF particle(i).parent > 0 THEN s = 4 'former gravitator cell
                    IF s < 2 THEN s = 2
                    CircleFill particle(i).x + camera, particle(i).y + cameraY, s, particle(i).color
                CASE GRAVITATORCELL
                    'show
                    FOR j = 6 TO 4 STEP -1
                        CircleFill particle(i).x + camera, particle(i).y + cameraY + gravitationalFloat, 6, _RGBA32(particle(i).red, particle(i).green, particle(i).blue, map(j, 4, 6, 10, 25))
                    NEXT
                    CircleFill particle(i).x + camera, particle(i).y + cameraY + gravitationalFloat, 3, _RGB32(255, 244, 238)
            END SELECT
        END IF
    NEXT
END SUB

SUB setRand (seed&)
    IF seed& > UBOUND(rndtable) OR seed& < 1 THEN
        rndIndex = 1
    ELSE
        rndIndex = seed&
    END IF
END SUB

FUNCTION getRND
    rndIndex = rndIndex + 1
    IF rndIndex > UBOUND(rndtable) THEN rndIndex = 1
    getRND = rndTable(rndIndex)
END FUNCTION

SUB slowMo
    frameRate = 20: frameRateRestoreTimer = TIMER
END SUB

SUB drawBG
    DIM i AS INTEGER
    FOR i = 1 TO maxBGDecoration
        IF TIMER - levelSet < respawnDelay THEN
            LINE (bg(i).x + camera / 2, bg(i).y + cameraY)-STEP(bg(i).w, bg(i).h), _RGBA32(bg(i).red, bg(i).green, bg(i).blue, map(TIMER - levelSet, 0, respawnDelay, 0, 255)), BF
        ELSE
            LINE (bg(i).x + camera / 2, bg(i).y + cameraY)-STEP(bg(i).w, bg(i).h), bg(i).color, BF
        END IF
    NEXT
END SUB

FUNCTION noise! (x AS SINGLE, y AS SINGLE, z AS SINGLE)
    STATIC p5NoiseSetup AS _BYTE
    STATIC perlin() AS SINGLE
    STATIC PERLIN_YWRAPB AS SINGLE, PERLIN_YWRAP AS SINGLE
    STATIC PERLIN_ZWRAPB AS SINGLE, PERLIN_ZWRAP AS SINGLE
    STATIC PERLIN_SIZE AS SINGLE, perlin_octaves AS SINGLE
    STATIC perlin_amp_falloff AS SINGLE

    IF NOT p5NoiseSetup THEN
        p5NoiseSetup = true

        PERLIN_YWRAPB = 4
        PERLIN_YWRAP = INT(1 * (2 ^ PERLIN_YWRAPB))
        PERLIN_ZWRAPB = 8
        PERLIN_ZWRAP = INT(1 * (2 ^ PERLIN_ZWRAPB))
        PERLIN_SIZE = 4095

        perlin_octaves = 4
        perlin_amp_falloff = 0.5

        REDIM perlin(PERLIN_SIZE + 1) AS SINGLE
        DIM i AS SINGLE
        FOR i = 0 TO PERLIN_SIZE + 1
            perlin(i) = RND
        NEXT
    END IF

    x = ABS(x)
    y = ABS(y)
    z = ABS(z)

    DIM xi AS SINGLE, yi AS SINGLE, zi AS SINGLE
    xi = INT(x)
    yi = INT(y)
    zi = INT(z)

    DIM xf AS SINGLE, yf AS SINGLE, zf AS SINGLE
    xf = x - xi
    yf = y - yi
    zf = z - zi

    DIM r AS SINGLE, ampl AS SINGLE, o AS SINGLE
    r = 0
    ampl = .5

    FOR o = 1 TO perlin_octaves
        DIM of AS SINGLE, rxf AS SINGLE
        DIM ryf AS SINGLE, n1 AS SINGLE, n2 AS SINGLE, n3 AS SINGLE
        of = xi + INT(yi * (2 ^ PERLIN_YWRAPB)) + INT(zi * (2 ^ PERLIN_ZWRAPB))

        rxf = 0.5 * (1.0 - COS(xf * _PI))
        ryf = 0.5 * (1.0 - COS(yf * _PI))

        n1 = perlin(of AND PERLIN_SIZE)
        n1 = n1 + rxf * (perlin((of + 1) AND PERLIN_SIZE) - n1)
        n2 = perlin((of + PERLIN_YWRAP) AND PERLIN_SIZE)
        n2 = n2 + rxf * (perlin((of + PERLIN_YWRAP + 1) AND PERLIN_SIZE) - n2)
        n1 = n1 + ryf * (n2 - n1)

        of = of + PERLIN_ZWRAP
        n2 = perlin(of AND PERLIN_SIZE)
        n2 = n2 + rxf * (perlin((of + 1) AND PERLIN_SIZE) - n2)
        n3 = perlin((of + PERLIN_YWRAP) AND PERLIN_SIZE)
        n3 = n3 + rxf * (perlin((of + PERLIN_YWRAP + 1) AND PERLIN_SIZE) - n3)
        n2 = n2 + ryf * (n3 - n2)

        n1 = n1 + (0.5 * (1.0 - COS(zf * _PI))) * (n2 - n1)

        r = r + n1 * ampl
        ampl = ampl * perlin_amp_falloff
        xi = INT(xi * (2 ^ 1))
        xf = xf * 2
        yi = INT(yi * (2 ^ 1))
        yf = yf * 2
        zi = INT(zi * (2 ^ 1))
        zf = zf * 2

        IF xf >= 1.0 THEN xi = xi + 1: xf = xf - 1
        IF yf >= 1.0 THEN yi = yi + 1: yf = yf - 1
        IF zf >= 1.0 THEN zi = zi + 1: zf = zf - 1
    NEXT
    noise! = r
END FUNCTION

