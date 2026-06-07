---
name: JSDoc
category: tool
contributors:
    - ["Jamie Magee", "https://github.com/JamieMagee"]
filename: jsdoc.js
---

JSDoc is a documentation generation tool for JavaScript, similar to Javadoc for Java.
It uses specially formatted comments to generate API documentation.
Created in 1999 by Michael Mathews, JSDoc has become the standard way to document JavaScript code.

JSDoc comments start with `/**` and end with `*/`.
Inside, you use special tags (starting with `@`) to provide structured information about your code.

```js
/**
 * This is a JSDoc comment block.
 * It describes what the following code does.
 */

////////////////////////////////////
// 1. Basic JSDoc Comment Structure
////////////////////////////////////

/**
 * A simple function that adds two numbers.
 * @param {number} a - The first number
 * @param {number} b - The second number
 * @returns {number} The sum of a and b
 */
function add(a, b) {
    return a + b;
}

/**
 * You can also document variables
 * @type {string}
 */
const userName = "John Doe";

/**
 * Constants work the same way
 * @constant {number}
 * @default 42
 */
const MEANING_OF_LIFE = 42;

////////////////////////////////////
// 2. Function Documentation
////////////////////////////////////

/**
 * Function with multiple parameters and detailed descriptions
 * @param {string} name - The user's name
 * @param {number} age - The user's age (must be positive)
 * @param {boolean} [isActive=true] - Whether the user is active (optional)
 * @returns {Object} User object with processed information
 * @throws {Error} Throws an error if age is negative
 * @example
 * // Create a new user
 * const user = createUser("Alice", 25, true);
 * console.log(user.name); // "Alice"
 */
function createUser(name, age, isActive = true) {
    if (age < 0) {
        throw new Error("Age cannot be negative");
    }

    return {
        name: name,
        age: age,
        isActive: isActive,
        createdAt: new Date()
    };
}

/**
 * Function with complex parameter types
 * @param {string|number} id - User ID (can be string or number)
 * @param {Object} options - Configuration options
 * @param {string} options.format - Output format ("json" or "xml")
 * @param {boolean} [options.includeMetadata=false] - Include metadata
 * @returns {Promise<string>} Promise that resolves to formatted user data
 */
async function getUserData(id, options) {
    // Implementation here
}

/**
 * Function with callback parameter
 * @param {Array} items - Array of items to process
 * @param {function(*, number): boolean} callback - Filter function
 * @returns {Array} Filtered array
 */
function filterItems(items, callback) {
    return items.filter(callback);
}

////////////////////////////////////
// 3. Class Documentation
////////////////////////////////////

/**
 * Represents a person
 * @class
 * @classdesc This class represents a person with basic information
 */
class Person {
    /**
     * Create a person
     * @param {string} name - The person's name
     * @param {number} age - The person's age
     */
    constructor(name, age) {
        /**
         * The person's name
         * @type {string}
         */
        this.name = name;

        /**
         * The person's age
         * @type {number}
         * @private
         */
        this._age = age;
    }

    /**
     * Get the person's age
     * @returns {number} The person's age
     */
    getAge() {
        return this._age;
    }

    /**
     * Set the person's age
     * @param {number} age - The new age
     * @throws {Error} If age is not a positive number
     */
    setAge(age) {
        if (typeof age !== 'number' || age < 0) {
            throw new Error('Age must be a positive number');
        }
        this._age = age;
    }

    /**
     * Static method to create a person from string
     * @static
     * @param {string} str - String in format "name,age"
     * @returns {Person} New Person instance
     */
    static fromString(str) {
        const [name, age] = str.split(',');
        return new Person(name, parseInt(age));
    }
}

/**
 * Employee class extending Person
 * @extends Person
 */
class Employee extends Person {
    /**
     * Create an employee
     * @param {string} name - Employee name
     * @param {number} age - Employee age
     * @param {string} department - Employee department
     */
    constructor(name, age, department) {
        super(name, age);

        /**
         * Employee department
         * @type {string}
         * @readonly
         */
        this.department = department;
    }

    /**
     * Get employee information
     * @override
     * @returns {Object} Employee information object
     */
    getInfo() {
        return {
            name: this.name,
            age: this.getAge(),
            department: this.department
        };
    }
}

////////////////////////////////////
// 4. Type Definitions and Interfaces
////////////////////////////////////

/**
 * User configuration object
 * @typedef {Object} UserConfig
 * @property {string} name - User's name
 * @property {number} age - User's age
 * @property {string[]} hobbies - List of hobbies
 * @property {boolean} [isActive=true] - Whether user is active
 */

/**
 * Create user with configuration
 * @param {UserConfig} config - User configuration
 * @returns {Object} Created user object
 */
function createUserFromConfig(config) {
    // Implementation using the UserConfig type
}

/**
 * Callback function type for array operations
 * @callback ArrayCallback
 * @param {*} item - Current array item
 * @param {number} index - Current index
 * @param {Array} array - Original array
 * @returns {boolean} Whether to keep the item
 */

/**
 * Filter array using callback
 * @param {Array} arr - Input array
 * @param {ArrayCallback} callback - Filter callback
 * @returns {Array} Filtered array
 */
function customFilter(arr, callback) {
    return arr.filter(callback);
}

////////////////////////////////////
// 5. Enums and Constants
////////////////////////////////////

/**
 * HTTP status codes
 * @enum {number}
 */
const HttpStatus = {
    /** Success */
    OK: 200,
    /** Resource not found */
    NOT_FOUND: 404,
    /** Internal server error */
    INTERNAL_ERROR: 500
};

/**
 * Color enumeration
 * @enum {string}
 */
const Colors = {
    RED: 'red',
    GREEN: 'green',
    BLUE: 'blue'
};

////////////////////////////////////
// 6. Modules and Namespaces
////////////////////////////////////

/**
 * @fileoverview Utility functions for string manipulation
 * @module StringUtils
 * @version 1.0.0
 * @author John Doe
 * @since 2025-01-01
 */

/**
 * String utilities namespace
 * @namespace
 */
const StringUtils = {
    /**
     * Capitalize first letter of string
     * @memberof StringUtils
     * @param {string} str - Input string
     * @returns {string} Capitalized string
     */
    capitalize: function(str) {
        return str.charAt(0).toUpperCase() + str.slice(1);
    },

    /**
     * Reverse a string
     * @memberof StringUtils
     * @param {string} str - Input string
     * @returns {string} Reversed string
     */
    reverse: function(str) {
        return str.split('').reverse().join('');
    }
};

////////////////////////////////////
// 7. Events and Mixins
////////////////////////////////////

/**
 * Event emitter class
 * @class
 * @mixes EventMixin
 */
class EventEmitter {
    /**
     * Emit an event
     * @fires EventEmitter#dataReceived
     * @param {string} eventName - Name of the event
     * @param {*} data - Event data
     */
    emit(eventName, data) {
        /**
         * Data received event
         * @event EventEmitter#dataReceived
         * @type {Object}
         * @property {string} type - Event type
         * @property {*} data - Event data
         */
        // Implementation here
    }

    /**
     * Listen for events
     * @listens EventEmitter#dataReceived
     * @param {string} eventName - Event name to listen for
     * @param {function} callback - Event callback
     */
    on(eventName, callback) {
        // Implementation here
    }
}

/**
 * Mixin for event functionality
 * @mixin
 */
const EventMixin = {
    /**
     * Add event listener
     * @param {string} event - Event name
     * @param {function} handler - Event handler
     */
    addEventListener: function(event, handler) {
        // Implementation
    }
};

////////////////////////////////////
// 8. Advanced Tags and Annotations
////////////////////////////////////

/**
 * Legacy function - use newFunction instead
 * @deprecated since version 2.0.0
 * @param {string} input - Input string
 * @returns {string} Processed string
 */
function oldFunction(input) {
    return input.toUpperCase();
}

/**
 * @todo Add input validation
 * @todo Implement caching mechanism
 * @todo Add unit tests
 */
function futureEnhancement() {
    // Implementation pending
}

/**
 * Internal function - not part of public API
 * @private
 * @param {string} secret - Secret value
 * @returns {string} Processed secret
 */
function _processSecret(secret) {
    return secret.split('').reverse().join('');
}

/**
 * Abstract method - must be implemented by subclass
 * @abstract
 * @param {*} data - Data to process
 * @returns {*} Processed data
 */
function processData(data) {
    throw new Error('Method must be implemented');
}

/**
 * Async function example
 * @async
 * @param {string} url - URL to fetch
 * @returns {Promise<Object>} Promise resolving to response data
 */
async function fetchData(url) {
    const response = await fetch(url);
    return response.json();
}

/**
 * Generator function
 * @generator
 * @yields {number} Sequential numbers
 */
function* numberGenerator() {
    let i = 0;
    while (true) {
        yield i++;
    }
}

////////////////////////////////////
// 9. External Libraries and Links
////////////////////////////////////

/**
 * jQuery reference
 * @external jQuery
 * @see {@link https://jquery.com/}
 */

/**
 * Function using external library
 * @param {external:jQuery} $ - jQuery object
 * @param {string} selector - CSS selector
 * @returns {external:jQuery} jQuery collection
 */
function enhanceElements($, selector) {
    return $(selector).addClass('enhanced');
}

/**
 * Function with multiple see references
 * @param {string} input - Input string
 * @returns {string} Processed string
 * @see {@link https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String}
 * @see {@link StringUtils.capitalize} for capitalization
 * @see {@tutorial string-processing} for more examples
 */
function processString(input) {
    return input.trim().toLowerCase();
}
```

## Further Reading

- [JSDoc Official Documentation](https://jsdoc.app/) - Complete reference for all JSDoc tags and features
- [JSDoc on GitHub](https://github.com/jsdoc/jsdoc) - Source code and issue tracking
- [Google JavaScript Style Guide](https://google.github.io/styleguide/jsguide.html) - Includes JSDoc conventions
- [TypeScript JSDoc Reference](https://www.typescriptlang.org/docs/handbook/jsdoc-supported-types.html) - JSDoc support in TypeScript
